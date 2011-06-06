varying vec3 normal, light_dir;

void main(void)
{
  vec4 p;
  light_dir = normalize(vec3(gl_LightSource[0].position));
  normal = normalize(gl_NormalMatrix * gl_Normal);

  gl_Position = ftransform();
}

