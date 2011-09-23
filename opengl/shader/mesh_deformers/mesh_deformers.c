/*!
 * \file    mesh_deformers.c
 * \brief   Testing mesh-deformers in glsl.
 *
 * Program uses GLEW to provide mechanisms for determining if GL_ARB_**_shader
 * extensions are supported on platform. This uses glut for window system.
 *
 * \sa      http://glew.sourceforge.net/
 *
 * \author  Ronan-Alexandre Cherrueau ronancherrueau{at}gmail{dot}com
 * \date    first release 23/09/2011
 */

#include "mesh_deformers.h"

void init_light(float x, float y, float z)
{
  // Lighting parameters:
  float light_ambient[4] = {.2, .2, .2, 1.};
  float light_diffuse[4] = {.8, .8, .8, 1.};
  float light_specular[4] = {.8, .8, .8, 1.};
  float light_position[4] = {x, y, z, 1.};

  glLightfv(GL_LIGHT0, GL_AMBIENT, light_ambient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, light_diffuse);
  glLightfv(GL_LIGHT0, GL_SPECULAR, light_specular);
  glLightfv(GL_LIGHT0, GL_POSITION, light_position);

  // Material parameters:
  float material_ambient[4] = {.2, .2, .2, 1.};
  float material_diffuse[4] = {1., 1., 1., 1.};
  float material_specular[4] = {1., 1., 1., 1.};
  float material_shininess = 100.;

  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, material_ambient);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, material_diffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, material_specular);
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, material_shininess);
}

void init_phong_point(void)
{
  shader_phong.vertex_shader = load_shader(GL_VERTEX_SHADER,
      "glsl/phong_point.vert");
  if(shader_phong.vertex_shader == 0) {
    exit_program();
  }
  
  shader_phong.fragment_shader = load_shader(GL_FRAGMENT_SHADER,
      "glsl/phong_point.frag");
  if(shader_phong.fragment_shader == 0) {
    exit_program();
  }

  shader_phong.program = make_program_from_two(
      shader_phong.vertex_shader, shader_phong.fragment_shader);
  if(shader_phong.program == 0) {
    exit_program();
  }
}

void init_mesh_deformers(void)
{
  shader_mdeformers.vertex_shader = load_shader(GL_VERTEX_SHADER,
      "glsl/deformers.vert");
  if(shader_mdeformers.vertex_shader == 0) {
    exit_program();
  }
  
  shader_mdeformers.fragment_shader = load_shader(GL_FRAGMENT_SHADER,
      "glsl/phong_point.frag");
  if(shader_mdeformers.fragment_shader == 0) {
    exit_program();
  }

  shader_mdeformers.program = make_program_from_two(
      shader_mdeformers.vertex_shader, shader_mdeformers.fragment_shader);
  if(shader_mdeformers.program == 0) {
    exit_program();
  }
}

void exit_program(void)
{
  glDeleteShader(shader_phong.vertex_shader);
  glDeleteShader(shader_phong.fragment_shader);
  glDeleteShader(shader_mdeformers.vertex_shader);
  glDeleteShader(shader_mdeformers.fragment_shader);
  glDeleteProgram(shader_phong.program);
  glDeleteProgram(shader_mdeformers.program);
}

void draw_cartesian_coordinates(void)
{
  GLUquadric *params = gluNewQuadric();

  gluQuadricDrawStyle(params, GL_FILL);
  gluQuadricTexture(params, GL_FALSE);

  // Z
  glTranslatef(0., 0., -2.);
  glColor3f(0., 1., 0.);
  gluCylinder(params, .02, .02, 4., 20., 1.); 
  glTranslatef(0., 0., 2.);

  // X
  glRotatef(90., 0., 1., 0.);
  glTranslatef(0., 0., -2.);
  glColor3f(1., 0., 0.);
  gluCylinder(params, .02, .02, 4., 20., 1.); 
  glTranslatef(0., 0., 2.);
  glRotatef(-90., 0., 1., 0.);

  // Y
  glRotatef(90., 1., 0., 0.);
  glTranslatef(0., 0., -2.);
  glColor3f(0., 0., 1.);
  gluCylinder(params, .02, .02, 4., 20., 1.); 
  glTranslatef(0., 0., 2.);
  glRotatef(-90., 1., 0., 0.);

  gluDeleteQuadric(params);
}

void draw_square(float width, float height, int slices)
{
  float new_width, new_height;
  float x, z;

  new_width = width / (float) slices;
  new_height = height / (float) slices;


  x = - (width / 2.);
  for(int i=0.; i<slices; ++i) {
    z = - (height / 2.);

    glBegin(GL_QUAD_STRIP);
    for(int j=0.; j<=slices; ++j) {
      glVertex3f(x, 0., z);
      glVertex3f(x + new_width, 0., z);

      z += new_height;
    }
    glEnd();

    x += new_width;
  }
}

void use_phong_shader(void)
{
  glUseProgram(shader_phong.program);
}

void use_mesh_deformers_shader(float amplitude, float frequency)
{
  glUseProgram(shader_mdeformers.program);
  glUniform1f(glGetUniformLocation(shader_mdeformers.program, "amplitude_"),
      amplitude);
  glUniform1f(glGetUniformLocation(shader_mdeformers.program, "frequency_"),
      frequency);
}

