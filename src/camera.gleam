import vector.{Vec3, Vector} as vec
import gleam/int
import gleam/iterator

pub type Ray {
  Ray(dir: Vector(Float), origin: Vector(Float))
}

pub type Camera {
  Camera(
    aspect_ratio: Float,
    vertical_fov: Float,
    position: Vector(Float),
    look_at: Vector(Float),
    up_dir: Vector(Float),
    defocus_angle: Float,
    focus_dist: Float,
    get_rays: fn(#(Float, Float), Int) -> List(Ray),
  )
}

@external(erlang, "math", "tan")
fn tangent(val: Float) -> Float

@external(erlang, "math", "pi")
fn pi() -> Float

pub fn new(
  aspect_ratio aspect_ratio: Float,
  image_width image_width: Int,
  vertical_fov vertical_fov: Float,
  position camera_position: Vector(Float),
  look_at look_at: Vector(Float),
  up_dir up_dir: Vector(Float),
  defocus_angle defocus_angle: Float,
  focus_dist focus_dist: Float,
) -> Camera {
  let image_width: Float = int.to_float(image_width)
  let image_height = image_width /. aspect_ratio
  let theta = vertical_fov *. pi() /. 180.0
  let h = tangent(theta /. 2.0)
  let viewport_height = 2.0 *. h *. focus_dist
  let viewport_width = viewport_height *. image_width /. image_height
  let w =
    vec.sub(camera_position, look_at)
    |> vec.normalize
  let u =
    vec.cross(up_dir, w)
    |> vec.normalize
  let v = vec.cross(w, u)
  let viewport_u =
    vec.splat3(viewport_width)
    |> vec.mul(u)
  let viewport_v =
    vec.splat3(viewport_height)
    |> vec.mul(vec.neg(v))
  let pixel_delta_u =
    viewport_u
    |> vec.div(vec.splat3(image_width))
  let pixel_delta_v =
    viewport_v
    |> vec.div(vec.splat3(image_height))
  let viewport_upper_left =
    camera_position
    |> vec.sub(vec.mul(w, vec.splat3(focus_dist)))
    |> vec.sub(vec.div(viewport_u, vec.splat3(2.0)))
    |> vec.sub(vec.div(viewport_v, vec.splat3(2.0)))
  let pixel00_loc =
    viewport_upper_left
    |> vec.add(
      vec.splat3(0.5)
      |> vec.mul(vec.add(pixel_delta_u, pixel_delta_v)),
    )
  let defocus_radius =
    focus_dist *. tangent({ defocus_angle /. 2.0 } *. pi() /. 180.0)
  let get_rays = fn(pixel_xy: #(Float, Float), rays_per_pixel: Int) {
    let pixel_x = pixel_xy.0
    let pixel_y = pixel_xy.1
    let pixel_center =
      pixel00_loc
      |> vec.add(vec.mul(pixel_delta_u, vec.splat3(pixel_x)))
      |> vec.add(vec.mul(pixel_delta_v, vec.splat3(pixel_y)))
    fn() {
      let ray_origin =
        case defocus_angle <=. 0.0 {
          True -> Vec3(0.0, 0.0, 0.0)
          _ -> {
            let defocus =
              vec.random_in_unit_2d()
              |> vec.mul_scalar(defocus_radius)
            vec.add(vec.mul_scalar(u, defocus.x), vec.mul_scalar(v, defocus.y))
          }
        }
        |> vec.add(camera_position)
      let antialiasing_offset = {
        let antialias =
          vec.random_in_unit_2d()
          |> vec.mul(vec.splat3(0.5))
        vec.add(
          vec.mul_scalar(pixel_delta_u, antialias.x),
          vec.mul_scalar(pixel_delta_v, antialias.y),
        )
      }
      let pixel_target = vec.add(antialiasing_offset, pixel_center)
      vec.sub(pixel_target, ray_origin)
      |> vec.normalize
      |> Ray(origin: ray_origin)
    }
    |> iterator.repeatedly
    |> iterator.take(rays_per_pixel)
    |> iterator.to_list
  }
  Camera(
    aspect_ratio: aspect_ratio,
    vertical_fov: vertical_fov,
    position: camera_position,
    look_at: look_at,
    up_dir: up_dir,
    get_rays: get_rays,
    defocus_angle: defocus_angle,
    focus_dist: focus_dist,
  )
}
