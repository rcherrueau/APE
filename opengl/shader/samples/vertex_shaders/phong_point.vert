varying vec3 normal, vertex_vector;

void main(void)
{
  normal = gl_NormalMatrix * gl_Normal;
  vertex_vector = normalize(vec3(gl_ModelViewMatrix * gl_Vertex));

  gl_Position = ftransform();
}

