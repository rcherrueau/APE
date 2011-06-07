varying vec3 normal, light_dir;

void main(void)
{
  vec3 vertex_user_space;

  vertex_user_space = vec3(gl_ModelViewMatrix * gl_Vertex);
  normal = gl_NormalMatrix * gl_Normal;
  light_dir = vec3(gl_LightSource[0].position) - vertex_user_space;
  gl_Position = ftransform();
}

