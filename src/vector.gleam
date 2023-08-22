import gleam/list
import gleam/int
import gleam/float
import gleam/result

pub type Vector(t) {
  Vec3(x: t, y: t, z: t)
}

pub fn splat3(a: a_type) {
  Vec3(a, a, a)
}

pub fn add(v1: Vector(Float), v2: Vector(Float)) {
  Vec3(v1.x +. v2.x, v1.y +. v2.y, v1.z +. v2.z)
}

pub fn sub(v1: Vector(Float), v2: Vector(Float)) {
  Vec3(v1.x -. v2.x, v1.y -. v2.y, v1.z -. v2.z)
}

pub fn mul(v1: Vector(Float), v2: Vector(Float)) {
  Vec3(v1.x *. v2.x, v1.y *. v2.y, v1.z *. v2.z)
}

pub fn mul_scalar(v1: Vector(Float), scalar: Float) {
  Vec3(v1.x *. scalar, v1.y *. scalar, v1.z *. scalar)
}

pub fn div(v1: Vector(Float), v2: Vector(Float)) {
  Vec3(v1.x /. v2.x, v1.y /. v2.y, v1.z /. v2.z)
}

pub fn neg(v: Vector(Float)) {
  Vec3(-1.0 *. v.x, -1.0 *. v.y, -1.0 *. v.z)
}

pub fn len(v: Vector(Float)) {
  let assert Ok(res) = float.square_root(v.x *. v.x +. v.y *. v.y +. v.z *. v.z)
  res
}

pub fn length_squared(v: Vector(Float)) {
  v.x *. v.x +. v.y *. v.y +. v.z *. v.z
}

pub fn normalize(v: Vector(Float)) {
  let len = len(v)
  Vec3(v.x /. len, v.y /. len, v.z /. len)
}

pub fn map(v: Vector(a), func: fn(a) -> b) {
  Vec3(func(v.x), func(v.y), func(v.z))
}

pub fn dot(v1: Vector(Float), v2: Vector(Float)) {
  v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z
}

pub fn cross(v1: Vector(Float), v2: Vector(Float)) {
  Vec3(
    v1.y *. v2.z -. v1.z *. v2.y,
    v1.z *. v2.x -. v1.x *. v2.z,
    v1.x *. v2.y -. v1.y *. v2.x,
  )
}

pub fn avg_list(vec_list: List(Vector(Float))) {
  let list_len =
    vec_list
    |> list.length
    |> int.to_float
  vec_list
  |> list.reduce(add)
  |> result.unwrap(splat3(0.0))
  |> div(splat3(list_len))
}

pub fn avg_iter(list_vec: List(Vector(Float))) {
  let list_len =
    list_vec
    |> list.length
    |> int.to_float
    |> splat3
  list_vec
  |> list.reduce(add)
  |> result.unwrap(splat3(0.0))
  |> div(list_len)
}

pub fn random(start: Float, stop: Float) {
  Vec3(
    float.random(start, stop),
    float.random(start, stop),
    float.random(start, stop),
  )
}

pub fn generate(f: fn() -> Float) {
  Vec3(f(), f(), f())
}

@external(erlang, "rand", "normal")
fn do_random_normal() -> Float

@external(erlang, "rand", "uniform")
fn do_random_uniform() -> Float

pub fn random_in_unit() {
  let a = Vec3(do_random_normal(), do_random_normal(), do_random_normal())
  let to_random_len = do_random_uniform() /. len(a)
  Vec3(a.x *. to_random_len, a.y *. to_random_len, a.z *. to_random_len)
}

pub fn random_in_unit_2d() {
  let a = Vec3(do_random_normal(), do_random_normal(), 0.0)
  let to_random_len = do_random_uniform() /. len(a)
  Vec3(a.x *. to_random_len, a.y *. to_random_len, 0.0)
}
