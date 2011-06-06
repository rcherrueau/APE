#include "shader_samples.h"

void trivial_shader(void)
{
  sc.vertex_shader = load_shader(GL_VERTEX_SHADER,
      "../samples/vertex_shaders/trivial_vertex_shader.vert");
  if(sc.vertex_shader == 0) {
    exit(0);
  }
  
  sc.fragment_shader = load_shader(GL_FRAGMENT_SHADER,
      "../samples/fragment_shaders/trivial_fragment_shader.frag");
  if(sc.fragment_shader == 0) {
    exit(0);
  }

  sc.program = make_program_from_two(sc.vertex_shader, sc.fragment_shader);
  if(sc.program == 0) {
    exit(0);
  }

  glUseProgram(sc.program);
}

void uniform_shader(void)
{
  sc.vertex_shader = load_shader(GL_VERTEX_SHADER,
      "../samples/vertex_shaders/uniform_vertex_shader.vert");
  if(sc.vertex_shader == 0) {
    exit(0);
  }
  
  sc.fragment_shader = load_shader(GL_FRAGMENT_SHADER,
      "../samples/fragment_shaders/uniform_fragment_shader.frag");
  if(sc.fragment_shader == 0) {
    exit(0);
  }

  sc.program = make_program_from_two(sc.vertex_shader, sc.fragment_shader);
  if(sc.program == 0) {
    exit(0);
  }

  glUseProgram(sc.program);

  // Uniform Variables
  float spec_intensity = 0.99;
  float spec_color[4] = {.8, .8, .8, 1.};
  float threshold[2] = {.5, .25};
  float colors[12] = {.4, .4, .8, 1.,
                      .2, .2, .4, 1.,
                      .1, .1, .1, 1.};
  glUniform1f(glGetUniformLocation(sc.program, "spec_intensity"),
      spec_intensity);

  glUniform4fv(glGetUniformLocation(sc.program, "spec_color"), 1, spec_color);

  glUniform1fv(glGetUniformLocation(sc.program, "t"), 2, threshold);

  glUniform4fv(glGetUniformLocation(sc.program, "colors"), 3, colors);
}

