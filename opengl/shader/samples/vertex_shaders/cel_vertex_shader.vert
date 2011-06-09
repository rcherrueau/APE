varying vec3 normal, light_dir;

/*!
 * To getting light direction, we have :
 * \li Vector OA (from center to vertex);
 * \li Vector OB (from center to ligthPosition);
 * 
 * So light direction = OB - OA = BA
 */
void main(void)
{
  vec3 vertex_vector, light_vector;

  normal = gl_NormalMatrix * gl_Normal;

  vertex_vector = vec3(gl_ModelViewMatrix * gl_Vertex);
  light_vector = vec3(gl_LightSource[0].position);
  light_dir = light_vector - vertex_vector;

  gl_Position = ftransform();
}

