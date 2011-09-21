/*!
 * \brief   Polygonal Mesh Deformers with Jacobian matrix.
 *
 * This vertex shader deforms the scene with a sinus function on Y. It needs two
 * uniforms float variable that are :
 *  \li 'amplitude' => which is the amplitude of signal ([-1;1] for sin);
 *  \li 'frequency' => which is the seep of signal (2*PI for sin);
 *
 * It's easy to displace a vertex, but the real difficulty is to find the normal
 * of new vertex. This vertex shader show how to compute normal vector with
 * jacobian matrix.
 *
 * \sa      http://www.ozone3d.net/tutorials/mesh_deformer_p2.php
 */
uniform float amplitude_, frequency_;

varying vec3 normal, vertex_vector;

/*!
 * \brief   Displace the position of vertex.
 *
 * \param   amplitude   The amplitude of signal.
 * \param   frequency   The frequency of signal.
 * \return  The new position of vertex.
 */
vec4 displace_vertex(vec4 current_vertex, float amplitude, float frequency)
{
  vec4 new_vertex;
  float dist;

  // Compute the distance of current_vertex from origine, without regarding
  // the Y axes.
  dist = sqrt(
      current_vertex.x * current_vertex.x +
      current_vertex.z * current_vertex.z);

  new_vertex = current_vertex;
  new_vertex.y = current_vertex.y + sin(dist * frequency) * amplitude;

  return new_vertex;
}

/*!
 * \brief   Compute the tangent vector.
 *
 * \return  The tangent vector of normal.
 */
vec3 compute_tangent(void)
{
  vec3 cross1, cross2, tangent;

  cross1 = cross(gl_Normal, vec3(0., 0., 1.));
  cross2 = cross(gl_Normal, vec3(1., 0., 0.));

  if(length(cross1) > length(cross2)) {
    tangent = cross1;
  } else {
    tangent = cross2;
  }

  return tangent;
}

/*!
 * \brief   Compute the binormal vector.
 *
 * \param   tangent   The tangent vector.
 * \return  The binormal vector of normal and tangent.
 */
vec3 compute_binormal(vec3 tangent)
{
  vec3 binormal;

  binormal = cross(gl_Normal, tangent);
  binormal = normalize(binormal);

  return binormal;
}

vec3 compute_normal(vec3 vertex, vec3 tangent, vec3 binormal, float amplitude,
    float frequency)
{
  mat3 J;
  float dist, jacobian_coef;

  dist = sqrt(vertex.x * vertex.x + vertex.z * vertex.z);
  jacobian_coef = cos(frequency * dist) / (dist + .00001 + amplitude);

  J[0][0] = 1.;
  J[0][1] = jacobian_coef * vertex.x;
  J[0][2] = 0.;

  J[1][0] = 0.;
  J[1][1] = 1.;
  J[1][2] = 0.;

  J[2][0] = 0.;
  J[2][1] = jacobian_coef * vertex.z;
  J[2][2] = 1.;

  return normalize(cross((J * tangent), (J * binormal)));
}

void main(void)
{
  vec4 new_vertex;
  vec3 tangent, binormal, new_normal;

  // Compute new position:
  new_vertex = displace_vertex(gl_Vertex, amplitude_, frequency_);
  vertex_vector = vec3(gl_ModelViewMatrix * new_vertex);
  gl_Position = gl_ModelViewProjectionMatrix * new_vertex;

  // Compute displaced normal
  tangent = compute_tangent();
  binormal = compute_binormal(tangent);
  new_normal = compute_normal(vec3(new_vertex), tangent, binormal, amplitude_,
      frequency_);

  normal = gl_NormalMatrix * new_normal;
}

