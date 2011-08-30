#include "shader_samples.h"

void trivial_shader(void)
{
  sc.vertex_shader = load_shader(GL_VERTEX_SHADER,
      "../samples/vertex_shaders/trivial.vert");
  if(sc.vertex_shader == 0) {
    exit(0);
  }
  
  sc.fragment_shader = load_shader(GL_FRAGMENT_SHADER,
      "../samples/fragment_shaders/trivial.frag");
  if(sc.fragment_shader == 0) {
    exit(0);
  }

  sc.program = make_program_from_two(sc.vertex_shader, sc.fragment_shader);
  if(sc.program == 0) {
    exit(0);
  }

  glUseProgram(sc.program);
}

void gouraud_shader(void)
{
  sc.vertex_shader = load_shader(GL_VERTEX_SHADER,
      "../samples/vertex_shaders/gouraud.vert");
  if(sc.vertex_shader == 0) {
    exit(0);
  }
  
  sc.fragment_shader = load_shader(GL_FRAGMENT_SHADER,
      "../samples/fragment_shaders/gouraud.frag");
  if(sc.fragment_shader == 0) {
    exit(0);
  }

  sc.program = make_program_from_two(sc.vertex_shader, sc.fragment_shader);
  if(sc.program == 0) {
    exit(0);
  }

  glUseProgram(sc.program);
}

void cel_shader(void)
{
  sc.vertex_shader = load_shader(GL_VERTEX_SHADER,
      "../samples/vertex_shaders/cel.vert");
  if(sc.vertex_shader == 0) {
    exit(0);
  }
  
  sc.fragment_shader = load_shader(GL_FRAGMENT_SHADER,
      "../samples/fragment_shaders/cel.frag");
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
      "../samples/vertex_shaders/uniform.vert");
  if(sc.vertex_shader == 0) {
    exit(0);
  }
  
  sc.fragment_shader = load_shader(GL_FRAGMENT_SHADER,
      "../samples/fragment_shaders/uniform.frag");
  if(sc.fragment_shader == 0) {
    exit(0);
  }

  sc.program = make_program_from_two(sc.vertex_shader, sc.fragment_shader);
  if(sc.program == 0) {
    exit(0);
  }

  glUseProgram(sc.program);

  // Uniform Variables
  float render_color[4] = {0., 1., 0., 1.};
  float threshold[3] = {.85, .5, .25};
  glUniform4fv(glGetUniformLocation(sc.program, "user_color"), 1, render_color);
  glUniform1fv(glGetUniformLocation(sc.program, "threshold"), 3, threshold);
}

void xray_shader(void)
{
  sc.vertex_shader = load_shader(GL_VERTEX_SHADER,
      "../samples/vertex_shaders/xray.vert");
  if(sc.vertex_shader == 0) {
    exit(0);
  }
  
  sc.fragment_shader = load_shader(GL_FRAGMENT_SHADER,
      "../samples/fragment_shaders/xray.frag");
  if(sc.fragment_shader == 0) {
    exit(0);
  }

  sc.program = make_program_from_two(sc.vertex_shader, sc.fragment_shader);
  if(sc.program == 0) {
    exit(0);
  }

  glUseProgram(sc.program);
}

void phong_shader(void)
{
  sc.vertex_shader = load_shader(GL_VERTEX_SHADER,
      "../samples/vertex_shaders/phong_point.vert");
  if(sc.vertex_shader == 0) {
    exit(0);
  }
  
  sc.fragment_shader = load_shader(GL_FRAGMENT_SHADER,
      "../samples/fragment_shaders/phong_point.frag");
  if(sc.fragment_shader == 0) {
    exit(0);
  }

  sc.program = make_program_from_two(sc.vertex_shader, sc.fragment_shader);
  if(sc.program == 0) {
    exit(0);
  }

  glUseProgram(sc.program);
}

