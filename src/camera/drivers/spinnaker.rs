//
// Vidoxide - Image acquisition for amateur astronomy
// Copyright (c) 2021 Filip Szczerek <ga.software@yahoo.com>
//
// This project is licensed under the terms of the MIT license
// (see the LICENSE file for details).
//

//!
//! Spinnaker camera driver.
//!

use crate::camera::*;
use ga_image;
use ga_image::Image;
use libspinnaker_sys::*;
use std::sync::Arc;

#[derive(Debug)]
pub enum SpinnakerError {
    Internal(_spinError),
    Error(String)
}

impl From<SpinnakerError> for CameraError {
    fn from(spinnaker_error: SpinnakerError) -> CameraError {
        CameraError::SpinnakerError(spinnaker_error)
    }
}

macro_rules! checked_call {
    ($func_call:expr) => {
        #[allow(non_upper_case_globals)]
        match unsafe { $func_call } {
            _spinError_SPINNAKER_ERR_SUCCESS => (),
            error => return Err(SpinnakerError::Internal(error).into())
        }
    }
}

/// Index in list of cameras.
type SpinnakerCameraId = usize;

impl From<CameraId> for SpinnakerCameraId {
    fn from(id: CameraId) -> SpinnakerCameraId {
        id.id1 as usize
    }
}

impl From<SpinnakerCameraId> for CameraId  {
    fn from(id: SpinnakerCameraId) -> CameraId {
        CameraId{ id1: id as u64, id2: 0 }
    }
}

/// Common GenICam camera feature names (Standard Features Naming Convention 1.5.1).
mod genicam {
    pub const ROOT:               &'static str = "Root";
    pub const DEVICE_VENDOR_NAME: &'static str = "DeviceVendorName";
    pub const DEVICE_MODEL_NAME:  &'static str = "DeviceModelName";
}

/// Wrappers of Spinnaker objects.
mod spin {
    use std::cell::RefCell;
    use libspinnaker_sys::*;
    use super::{CameraError, SpinnakerError};

    /// Calls the string `getter` on `object`.
    pub fn read_string<Object>(
        object: Object,
        getter: unsafe extern "C" fn(Object, *mut ::std::os::raw::c_char, *mut size_t) -> spinError
    ) -> Result<String, CameraError> {
        const MAX_BUF_LEN: usize = 256;
        let mut buf: [std::os::raw::c_char; MAX_BUF_LEN] = [0; MAX_BUF_LEN];
        let mut returned_buf_len: size_t = MAX_BUF_LEN as size_t;
        checked_call!(getter(object, (&mut buf).as_mut_ptr(), &mut returned_buf_len));
        assert!(returned_buf_len as usize <= MAX_BUF_LEN);
        if returned_buf_len == 0 {
            return Ok("".to_string());
        }

        let bytes: Vec<u8> = buf[..returned_buf_len as usize - 1].iter().map(|signed| *signed as u8).collect();
        Ok(String::from_utf8(bytes).unwrap())
    }

    pub struct System {
        handle: *mut std::os::raw::c_void
    }

    impl System {
        pub fn new() -> Result<System, CameraError> {
            let mut handle = std::ptr::null_mut();
            checked_call!(spinSystemGetInstance(&mut handle));
            Ok(System{ handle })
        }

        pub fn get(&mut self) -> *mut std::os::raw::c_void {
            self.handle
        }
    }

    impl Drop for System {
        fn drop(&mut self) {
            unsafe { spinSystemReleaseInstance(self.handle); }
        }
    }

    pub struct CameraList {
        handle: *mut std::os::raw::c_void
    }

    impl CameraList {
        pub fn new() -> Result<CameraList, CameraError> {
            let mut handle = std::ptr::null_mut();
            checked_call!(spinCameraListCreateEmpty(&mut handle));
            Ok(CameraList{ handle })
        }

        pub fn get(&mut self) -> *mut std::os::raw::c_void {
            self.handle
        }

        pub fn num_cameras(&self) -> Result<usize, CameraError> {
            let mut result = std::mem::MaybeUninit::uninit();
            checked_call!(spinCameraListGetSize(self.handle, result.as_mut_ptr()));
            let result = unsafe { result.assume_init() };
            Ok(result as usize)
        }

        pub fn camera(&self, index: usize) -> Result<Camera, CameraError> {
            let mut handle = std::ptr::null_mut();
            checked_call!(spinCameraListGet(self.handle, index as size_t, &mut handle));
            Ok(Camera{ handle, initialized: false })
        }
    }

    impl Drop for CameraList {
        fn drop(&mut self) {
            unsafe { spinCameraListDestroy(self.handle); }
        }
    }

    enum ImageDropMode {
        Release,
        Destroy
    }

    pub struct Image {
        handle: *mut std::os::raw::c_void,
        drop_mode: ImageDropMode
    }

    impl Image {
        pub fn status(&self) -> Result<spinImageStatus, CameraError> {
            let mut result = std::mem::MaybeUninit::uninit();
            checked_call!(spinImageGetStatus(self.handle, result.as_mut_ptr()));
            let result = unsafe { result.assume_init() };
            Ok(result)
        }

        pub fn pixel_format(&self) -> Result<spinPixelFormatEnums, CameraError> {
            let mut result = std::mem::MaybeUninit::uninit();
            checked_call!(spinImageGetPixelFormat(self.handle, result.as_mut_ptr()));
            let result = unsafe { result.assume_init() };
            Ok(result)
        }

        pub fn width(&self) -> Result<size_t, CameraError> {
            let mut result = std::mem::MaybeUninit::uninit();
            checked_call!(spinImageGetWidth(self.handle, result.as_mut_ptr()));
            let result = unsafe { result.assume_init() };
            Ok(result)
        }

        pub fn height(&self) -> Result<size_t, CameraError> {
            let mut result = std::mem::MaybeUninit::uninit();
            checked_call!(spinImageGetHeight(self.handle, result.as_mut_ptr()));
            let result = unsafe { result.assume_init() };
            Ok(result)
        }

        pub fn stride(&self) -> Result<size_t, CameraError> {
            let mut result = std::mem::MaybeUninit::uninit();
            checked_call!(spinImageGetStride(self.handle, result.as_mut_ptr()));
            let result = unsafe { result.assume_init() };
            Ok(result)
        }

        pub fn data_ptr(&self) -> Result<*const ::std::os::raw::c_void, CameraError> {
            let mut result = std::mem::MaybeUninit::uninit();
            checked_call!(spinImageGetData(self.handle, result.as_mut_ptr()));
            let result = unsafe { result.assume_init() };
            Ok(result)
        }

        pub fn data_size(&self) -> Result<size_t, CameraError> {
            let mut result = std::mem::MaybeUninit::uninit();
            checked_call!(spinImageGetBufferSize(self.handle, result.as_mut_ptr()));
            let result = unsafe { result.assume_init() };
            Ok(result)
        }
    }

    impl Drop for Image {
        fn drop(&mut self) {
            match self.drop_mode {
                ImageDropMode::Release => unsafe { spinImageRelease(self.handle); },
                ImageDropMode::Destroy => unsafe { spinImageDestroy(self.handle); }
            }
        }
    }

    pub struct Camera {
        handle: *mut std::os::raw::c_void,
        initialized: bool
    }

    impl Camera {
        pub fn next_image(&self) -> Result<Image, CameraError> {
            let mut handle = std::ptr::null_mut();
            checked_call!(spinCameraGetNextImageEx(self.handle, 1000, &mut handle));
            Ok(Image{ handle, drop_mode: ImageDropMode::Release })
        }

        pub fn init(&mut self) -> Result<(), CameraError> {
            checked_call!(spinCameraInit(self.handle));
            self.initialized = true;
            Ok(())
        }

        pub fn deinit(&mut self) -> Result<(), CameraError> {
            checked_call!(spinCameraDeInit(self.handle));
            self.initialized = false;
            Ok(())
        }

        pub fn device_node_map(&self) -> Result<NodeMap, CameraError> {
            let mut handle = std::ptr::null_mut();
            checked_call!(spinCameraGetTLDeviceNodeMap(self.handle, &mut handle));
            Ok(NodeMap{ handle })
        }

        pub fn stream_node_map(&self) -> Result<NodeMap, CameraError> {
            let mut handle = std::ptr::null_mut();
            checked_call!(spinCameraGetTLStreamNodeMap(self.handle, &mut handle));
            Ok(NodeMap{ handle })
        }

        /// Requires initialization via `init`.
        pub fn genicam_node_map(&self) -> Result<NodeMap, CameraError> {
            let mut handle = std::ptr::null_mut();
            checked_call!(spinCameraGetNodeMap(self.handle, &mut handle));
            Ok(NodeMap{ handle })
        }

        pub fn begin_acquisition(&mut self) -> Result<(), CameraError> {
            checked_call!(spinCameraBeginAcquisition(self.handle));
            Ok(())
        }

        pub fn end_acquisition(&mut self) -> Result<(), CameraError> {
            checked_call!(spinCameraEndAcquisition(self.handle));
            Ok(())
        }
    }

    impl Drop for Camera {
        fn drop(&mut self) {
            let _ = self.end_acquisition();
            let _ = self.deinit();
            unsafe { spinCameraRelease(self.handle); }
        }
    }

    pub struct NodeMap {
        handle: *mut std::os::raw::c_void
    }

    impl NodeMap {
        pub fn node(&self, name: &str) -> Result<Node, CameraError> {
            let mut handle = std::ptr::null_mut();
            checked_call!(spinNodeMapGetNode(self.handle, std::ffi::CString::new(name).unwrap().as_c_str().as_ptr(), &mut handle));

            let mut available: bool8_t = False;
            checked_call!(spinNodeIsAvailable(handle, &mut available));
            if available != True {
                return Err(SpinnakerError::Error(format!("Node \"{}\" is not available.", name)).into());
            }

            Ok(Node{ handle, name: name.to_string(), node_type: RefCell::new(None) })
        }
    }

    pub struct Node {
        handle: *mut std::os::raw::c_void,
        name: String,
        node_type: RefCell<Option<_spinNodeType>>
    }

    impl Node {
        pub fn node_type(&self) -> Result<_spinNodeType, CameraError> {
            if self.node_type.borrow().is_some() {
                Ok(self.node_type.borrow().unwrap())
            } else {
                let mut node_type = std::mem::MaybeUninit::uninit();
                checked_call!(spinNodeGetType(self.handle, node_type.as_mut_ptr()));
                let node_type = unsafe { node_type.assume_init() };
                self.node_type.replace(Some(node_type));
                Ok(node_type)
            }
        }

        pub fn string_value(&self) -> Result<String, CameraError> {
            let mut readable: bool8_t = False;
            checked_call!(spinNodeIsReadable(self.handle, &mut readable));
            if readable != True {
                return Err(SpinnakerError::Error(format!("Node \"{}\" is not readable.", self.name)).into());
            }

            read_string(self.handle, spinStringGetValue)
        }

        pub fn as_string(&self) -> Result<String, CameraError> {
            let mut readable: bool8_t = False;
            checked_call!(spinNodeIsReadable(self.handle, &mut readable));
            if readable != True {
                return Err(SpinnakerError::Error(format!("Node \"{}\" is not readable.", self.name)).into());
            }

            read_string(self.handle, spinNodeToString)
        }

        pub fn name(&self) -> &str {
            &self.name
        }

        pub fn display_name(&self) -> Result<String, CameraError> {
            read_string(self.handle, spinNodeGetDisplayName)
        }

        pub fn num_children(&self) -> Result<size_t, CameraError> {
            if self.node_type()? != _spinNodeType_CategoryNode {
                return Ok(0)
            }

            let mut result: size_t = 0;
            checked_call!(spinCategoryGetNumFeatures(self.handle, &mut result));
            Ok(result)
        }

        pub fn child(&self, index: size_t) -> Result<Node, CameraError> {
            let mut handle = std::ptr::null_mut();
            checked_call!(spinCategoryGetFeatureByIndex(self.handle, index, &mut handle));
            let mut child = Node{ handle, name: "".to_string(), node_type: RefCell::new(None) };
            child.name = read_string(child.handle, spinNodeGetName)?;
            Ok(child)
        }
    }
}

fn to_pix_format(pix_format: _spinPixelFormatEnums) -> ga_image::PixelFormat {
    match pix_format {
        _spinPixelFormatEnums_PixelFormat_Mono8 => ga_image::PixelFormat::Mono8,
        _spinPixelFormatEnums_PixelFormat_Mono16 => ga_image::PixelFormat::Mono16,

        _ => panic!("Unsupported Spinnaker pixel format: {}", pix_format)
    }
}

pub struct SpinnakerDriver {
    system: spin::System,
    cameras: Option<spin::CameraList>
}

impl SpinnakerDriver {
    pub fn new() -> Result<SpinnakerDriver, CameraError> {
        let mut system = spin::System::new()?;

        let mut lib_ver = std::mem::MaybeUninit::uninit();
        checked_call!(spinSystemGetLibraryVersion(system.get(), lib_ver.as_mut_ptr()));
        let lib_ver = unsafe { lib_ver.assume_init() };
        println!("Spinnaker version: {}.{}.{}.{}", lib_ver.major, lib_ver.minor, lib_ver.type_, lib_ver.build);

        Ok(SpinnakerDriver{ system, cameras: None })
    }
}

impl Driver for SpinnakerDriver {
    fn name(&self) -> &'static str { "Spinnaker" }

    fn enumerate_cameras(&mut self) -> Result<Vec<CameraInfo>, CameraError> {
        let mut camera_list = spin::CameraList::new()?;
        checked_call!(spinSystemGetCameras(self.system.get(), camera_list.get()));

        let mut result = vec![];

        for i in 0..camera_list.num_cameras()? {
            let camera = camera_list.camera(i)?;
            let node_map = camera.device_node_map()?;
            result.push(CameraInfo{
                id: i.into(),
                name: format!("{}", node_map.node(genicam::DEVICE_MODEL_NAME)?.string_value()?)
            });
        }

        self.cameras = Some(camera_list);

        Ok(result)
    }

    /// Returns camera with acquisition enabled.
    fn open_camera(&mut self, id: CameraId) -> Result<Box<dyn Camera>, CameraError> {
        let index: usize = id.into();
        if self.cameras.is_none() || index > self.cameras.as_ref().unwrap().num_cameras()? {
            return Err(SpinnakerError::Error(format!("Invalid camera id: {}", index)).into());
        }

        let mut camera_handle = self.cameras.as_ref().unwrap().camera(index)?;
        camera_handle.init()?;
        camera_handle.begin_acquisition()?;

        Ok(Box::new(SpinnakerCamera{ camera_handle: Arc::new(camera_handle), id }))
    }
}

struct SpinnakerCamera {
    id: CameraId,
    camera_handle: Arc<spin::Camera>
}

impl Camera for SpinnakerCamera {
    fn id(&self) -> CameraId {
        self.id
    }

    fn name(&self) -> &str {
        unimplemented!()
    }

    fn enumerate_controls(&mut self) -> Result<Vec<CameraControl>, CameraError> {
        Ok(vec![])
    }

    fn create_capturer(&self) -> Result<Box<dyn FrameCapturer + Send>, CameraError> {
        // The returned frame capturer will share `camera_handle` (via `Arc`). Spinnaker allows using its functions
        // from multiple threads without additional synchronization. The `SpinnakerCamera`'s instance will be used
        // by the main thread, and the `FrameCapturer`'s instance - by the capture thread.
        Ok(Box::new(SpinnakerFrameCapturer{
            camera_handle: self.camera_handle.clone(),
        }))
    }

    fn set_number_control(&self, id: CameraControlId, value: f64) -> Result<Vec<Notification>, CameraError> {
        unimplemented!()
    }

    fn set_list_control(&mut self, id: CameraControlId, option_idx: usize) -> Result<Vec<Notification>, CameraError> {
        unimplemented!()
    }

    fn set_auto(&self, id: CameraControlId, state: bool) -> Result<Vec<Notification>, CameraError> {
        unimplemented!()
    }

    fn set_on_off(&self, id: CameraControlId, state: bool) -> Result<Vec<Notification>, CameraError> {
        unimplemented!()
    }

    fn get_number_control(&self, id: CameraControlId) -> Result<f64, CameraError> {
        unimplemented!()
    }

    fn get_list_control(&self, id: CameraControlId) -> Result<usize, CameraError> {
        unimplemented!()
    }

    fn temperature(&self) -> Option<f64> {
        None
    }

    fn set_roi(&mut self, x0: u32, y0: u32, width: u32, height: u32) -> Result<(), CameraError> {
        unimplemented!()
    }

    fn unset_roi(&mut self) -> Result<(), CameraError> {
        unimplemented!()
    }
}

struct SpinnakerFrameCapturer {
    camera_handle: Arc<spin::Camera>
}

unsafe impl Send for SpinnakerFrameCapturer {}

impl FrameCapturer for SpinnakerFrameCapturer {
    fn capture_frame(&mut self, dest_image: &mut Image) -> Result<(), CameraError> {
        let frame = self.camera_handle.next_image()?;
        let status = frame.status()?;
        if status != _spinImageStatus_IMAGE_NO_ERROR {
            return Err(SpinnakerError::Error(spin::read_string(status, spinImageGetStatusDescription)?).into());
        }

        let f_width = frame.width()? as u32;
        let f_height = frame.height()? as u32;
        let f_stride = frame.stride()? as usize;
        let f_pix_fmt = to_pix_format(frame.pixel_format()?);

        let frame_pixels: &[u8] = unsafe { std::slice::from_raw_parts(
            frame.data_ptr()? as *const u8,
            frame.data_size()? as usize
        ) };

        if dest_image.bytes_per_line() != f_stride ||
            dest_image.width() != f_width ||
            dest_image.height() != f_height ||
            dest_image.pixel_format() != f_pix_fmt {

            *dest_image = Image::new_from_pixels(
                f_width,
                f_height,
                Some(f_stride),
                f_pix_fmt,
                None,
                frame_pixels.to_vec()
            );
        } else {
            dest_image.raw_pixels_mut().copy_from_slice(frame_pixels);
        }

        // TODO?
        // if self.is_machine_big_endian ^ (frame.little_endian != dc1394bool_t::DC1394_TRUE) {
        //     dest_image.reverse_byte_order();
        // }

        Ok(())
    }

    fn pause(&mut self) {
        unimplemented!()
    }

    fn resume(&mut self) {
        unimplemented!()
    }
}
