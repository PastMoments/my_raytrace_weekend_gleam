import gleam/io
import gleam/iterator
import gleam/list
import gleam/int
import gleam/float
import vector.{Vec3, Vector} as vec
import gleam/option.{None, Option, Some}
import camera.{Ray}
import gleam/string

type Material {
  Material(scatter: fn(Ray, Hit) -> Option(Ray), colour: Vector(Float))
}

// hit_fn: ray -> distance
// normal_fn: hit pos -> normal vec
type Hittable {
  Hittable(
    hit_fn: fn(Ray) -> Option(Float),
    normal_fn: fn(Vector(Float)) -> Vector(Float),
    material: Material,
  )
}

type Hit {
  Hit(
    ray: Ray,
    dist: Float,
    pos: Vector(Float),
    normal: Vector(Float),
    obj: Hittable,
  )
}

fn apply_hit(obj: Hittable, ray: Ray) -> Option(Hit) {
  case obj.hit_fn(ray) {
    Some(dist) -> {
      let hit_pos =
        Vec3(
          ray.dir.x *. dist +. ray.origin.x,
          ray.dir.y *. dist +. ray.origin.y,
          ray.dir.z *. dist +. ray.origin.z,
        )
      let normal = obj.normal_fn(hit_pos)
      Hit(ray: ray, dist: dist, pos: hit_pos, normal: normal, obj: obj)
      |> Some
    }
    _ -> None
  }
}

@external(erlang, "rand", "uniform")
fn do_random_uniform() -> Float

fn sphere(centre: Vector(Float), radius: Float, material: Material) -> Hittable {
  let radius_squared = radius *. radius
  let hit_distance = fn(ray: Ray) -> Option(Float) {
    let neg_oc = vec.sub(centre, ray.origin)
    let neg_half_b = vec.dot(neg_oc, ray.dir)
    let c = vec.length_squared(neg_oc) -. radius_squared
    let discriminant = neg_half_b *. neg_half_b -. c
    case discriminant >=. 0.0 {
      True -> {
        let assert Ok(discr_sqrt) = float.square_root(discriminant)
        let root1 = neg_half_b -. discr_sqrt
        let root2 = neg_half_b +. discr_sqrt
        case root1 >. 0.0001, root2 >. 0.0001 {
          True, _ -> Some(root1)
          _, True -> Some(root2)
          _, _ -> None
        }
      }
      _ -> None
    }
  }
  let vec_radius = vec.splat3(radius)
  let sphere_normal = fn(hit_pos: Vector(Float)) {
    hit_pos
    |> vec.sub(centre)
    |> vec.mul(vec_radius)
    |> vec.normalize
  }
  Hittable(hit_fn: hit_distance, normal_fn: sphere_normal, material: material)
}

fn reflect(ray_dir: Vector(Float), normal: Vector(Float)) {
  ray_dir
  |> vec.sub({
    vec.dot(ray_dir, normal)
    |> vec.splat3
    |> vec.mul(normal)
    |> vec.mul(vec.splat3(2.0))
  })
  |> vec.normalize
}

fn refract(
  ray_dir: Vector(Float),
  normal: Vector(Float),
  refraction_ratio: Float,
) {
  let cos_theta =
    vec.neg(ray_dir)
    |> vec.dot(normal)
    |> float.min(1.0)
    |> vec.splat3
  let r_out_perp =
    cos_theta
    |> vec.mul(normal)
    |> vec.add(ray_dir)
    |> vec.mul(vec.splat3(refraction_ratio))
  let r_out_parallel =
    r_out_perp
    |> vec.length_squared
    |> fn(x) {
      let assert Ok(result) =
        float.absolute_value(1.0 -. x)
        |> float.square_root
      result
    }
    |> vec.splat3
    |> vec.mul(normal)
  vec.sub(r_out_perp, r_out_parallel)
  |> vec.normalize
}

@external(erlang, "math", "pow")
fn do_power(a: Float, b: Float) -> Float

fn dielectric_scatter(refraction_ratio: Float) -> fn(Ray, Hit) -> Option(Ray) {
  fn(ray: Ray, hit: Hit) -> Option(Ray) {
    let outer_face = vec.dot(ray.dir, hit.normal) <. 0.0
    let #(refraction_ratio, refraction_normal) = case outer_face {
      True -> #(1.0 /. refraction_ratio, hit.normal)
      _ -> #(refraction_ratio, vec.neg(hit.normal))
    }
    let cos_theta =
      vec.neg(ray.dir)
      |> vec.dot(refraction_normal)
      |> float.min(1.0)
    let assert Ok(sin_theta) = float.square_root(1.0 -. cos_theta *. cos_theta)
    let internal_reflection = refraction_ratio *. sin_theta >. 1.0
    // Schlick's approximation
    let reflectance = {
      let r0 = { 1.0 -. refraction_ratio } /. { 1.0 +. refraction_ratio }
      let r0 = r0 *. r0
      r0 +. { 1.0 -. r0 } *. do_power(1.0 -. cos_theta, 5.0)
    }
    let scattered_ray = case
      internal_reflection || reflectance >. do_random_uniform()
    {
      True -> reflect(ray.dir, refraction_normal)
      _ -> refract(ray.dir, refraction_normal, refraction_ratio)
    }
    Ray(dir: scattered_ray, origin: hit.pos)
    |> Some
  }
}

fn metal_scatter(fuzz: Float) -> fn(Ray, Hit) -> Option(Ray) {
  fn(ray: Ray, hit: Hit) -> Option(Ray) {
    let reflected = reflect(ray.dir, hit.normal)
    let scattered =
      reflected
      |> vec.add({
        vec.random_in_unit()
        |> vec.normalize()
        |> vec.mul(vec.splat3(fuzz))
      })
      |> vec.normalize
    case vec.dot(scattered, hit.normal) >. 0.0 {
      True -> {
        Ray(dir: scattered, origin: hit.pos)
        |> Some
      }
      _ -> None
    }
  }
}

fn lambertian_scatter(_ray: Ray, hit: Hit) -> Option(Ray) {
  let scatter_dir =
    vec.random_in_unit()
    |> vec.normalize
    |> vec.add(hit.normal)
    |> vec.normalize
  let scatter_ray = Ray(dir: scatter_dir, origin: hit.pos)
  case vec.length_squared(scatter_ray.dir) >. 1.0e-6 {
    True -> Some(scatter_ray)
    _ -> None
  }
}

fn render_ray(ray: Ray, hittables: List(Hittable), bounces_left: Int) {
  let closer_hit_option = fn(hit1: Option(Hit), hit2: Option(Hit)) -> Option(
    Hit,
  ) {
    case hit1, hit2 {
      Some(h1), Some(h2) ->
        case h1.dist <. h2.dist {
          True -> hit1
          False -> hit2
        }
      None, _ -> hit2
      _, _ -> hit1
    }
  }
  // TODO: get this to work
  let closer_hit = fn(hit1: Hit, hit2: Hit) -> Hit {
    case hit1.dist <. hit1.dist {
      True -> hit1
      False -> hit2
    }
  }
  // main logic
  case bounces_left <= 0 {
    True -> vec.splat3(0.0)
    _ -> {
      let assert Ok(closest_hit) =
        hittables
        |> list.map(apply_hit(_, ray))
        //|> list.filter_map(option.to_result(_, Nil))
        |> list.reduce(closer_hit_option)
      case closest_hit {
        Some(closest_hit) -> {
          let material = closest_hit.obj.material
          case material.scatter(ray, closest_hit) {
            Some(ray) -> {
              ray
              |> render_ray(hittables, bounces_left - 1)
              |> vec.mul(material.colour)
            }
            None -> vec.splat3(0.0)
          }
        }
        _ -> {
          // sky
          let a = 0.5 *. { ray.dir.y +. 1.0 }
          Vec3(0.5, 0.7, 1.0)
          |> vec.mul(vec.splat3(a))
          |> vec.add(vec.splat3(1.0 -. a))
        }
      }
    }
  }
}

fn rgb_to_ppm_line(rgb: Vector(Float)) -> String {
  let rgb_str =
    rgb
    |> vec.mul(vec.splat3(255.99))
    |> vec.map(float.truncate)
    |> vec.map(int.to_string)
  [rgb_str.x, rgb_str.y, rgb_str.z]
  |> string.join(" ")
}

fn ppm_header(image_width: Int, image_height: Int) -> List(String) {
  [
    "P3",
    int.to_string(image_width) <> " " <> int.to_string(image_height),
    "255",
  ]
}

fn linear_to_gamma(linear_component: Float) {
  let assert Ok(result) = float.square_root(linear_component)
  result
}

pub fn main() {
  let material_ground = Material(lambertian_scatter, Vec3(0.5, 0.5, 0.5))
  let material_1 = Material(dielectric_scatter(1.5), Vec3(1.0, 1.0, 1.0))
  let material_2 = Material(lambertian_scatter, Vec3(0.4, 0.2, 0.1))
  let material_3 = Material(metal_scatter(0.0), Vec3(0.7, 0.6, 0.5))
  let hittables: List(Hittable) =
    [
      sphere(Vec3(0.0, -1000.0, 0.0), 1000.0, material_ground),
      sphere(Vec3(0.0, 1.0, 0.0), 1.0, material_1),
      sphere(Vec3(-4.0, 1.0, 0.0), 1.0, material_2),
      sphere(Vec3(4.0, 1.0, 0.0), 1.0, material_3),
    ]
    |> list.append(
      {
        use a <- list.flat_map(list.range(-11, 11 - 1))
        use b <- list.map(list.range(-11, 11 - 1))
        let choose_mat = do_random_uniform()
        let centre =
          Vec3(
            int.to_float(a) +. 0.9 *. do_random_uniform(),
            0.2,
            int.to_float(b) +. 0.9 *. do_random_uniform(),
          )
        case vec.len(vec.sub(centre, Vec3(4.0, 0.2, 0.0))) >. 0.9 {
          True if choose_mat <. 0.8 -> {
            let albedo = vec.mul(vec.random(0.0, 1.0), vec.random(0.0, 1.0))
            let material = Material(lambertian_scatter, albedo)
            Some(sphere(centre, 0.2, material))
          }
          True if choose_mat <. 0.95 -> {
            let albedo = vec.random(0.5, 1.0)
            let fuzz = float.random(0.0, 0.5)
            let material = Material(metal_scatter(fuzz), albedo)
            Some(sphere(centre, 0.2, material))
          }
          True -> {
            let material =
              Material(dielectric_scatter(1.5), Vec3(1.0, 1.0, 1.0))
            Some(sphere(centre, 0.2, material))
          }
          _ -> None
        }
      }
      |> list.filter_map(option.to_result(_, Nil)),
    )
  let aspect_ratio = 16.0 /. 9.0
  let image_width: Int = 1200
  let image_height = float.round(int.to_float(image_width) /. aspect_ratio)
  let rays_per_pixel = 500
  let max_bounce = 50
  let cam: camera.Camera =
    camera.new(
      aspect_ratio: aspect_ratio,
      image_width: image_width,
      vertical_fov: 20.0,
      position: Vec3(13.0, 2.0, 3.0),
      look_at: Vec3(0.0, 0.0, 0.0),
      up_dir: Vec3(0.0, 1.0, 0.0),
      defocus_angle: 0.6,
      focus_dist: 10.0,
    )
  let xy_list =
    {
      let width_range = iterator.range(from: 0, to: image_width - 1)
      let height_range = iterator.range(from: 0, to: image_height - 1)
      use y <- iterator.flat_map(height_range)
      use x <- iterator.map(width_range)
      #(int.to_float(x), int.to_float(y))
    }
    |> iterator.to_list
  let pixel_xy_to_ppm_line = fn(pixel_xy) {
    pixel_xy
    |> cam.get_rays(rays_per_pixel)
    |> list.map(render_ray(_, hittables, max_bounce))
    |> vec.avg_iter
    |> vec.map(linear_to_gamma)
    |> rgb_to_ppm_line
  }
  ppm_header(image_width, image_height)
  |> list.append(list.map(xy_list, pixel_xy_to_ppm_line))
  |> list.map(io.println)
}
