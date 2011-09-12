/*!
 * \file    main.c
 * \brief   Testing mesh-deformers in glsl.
 *
 * Program uses GLEW to provide mechanisms for determining if GL_ARB_**_shader
 * extensions are supported on platform. This uses glut for window system.
 *
 * \sa      http://glew.sourceforge.net/
 *
 * \author  Ronan-Alexandre Cherrueau ronancherrueau{at}gmail{dot}com
 * \date    first release 12/09/2011
 */

#include <stdlib.h>
#include <stdio.h>

#include <GL/glew.h>

#ifdef __APPLE__
  #include <GLUT/glut.h>
#else
  #include <GL/glut.h>
#endif

#include "utils/shader_compiling.h"

enum AXES {X = 0, Y, Z, H};

void init()
{
	glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  
  // To activate glColor when lightning is enabled = Color Tracking
  glEnable(GL_COLOR_MATERIAL);

  // To enable alpha transparency
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  // Antialiasing
  glEnable(GL_SMOOTH);
}

void init_light(void)
{
  // Lighting parameters:
  float light_ambient[4] = {.2, .2, .2, 1.};
  float light_diffuse[4] = {1., 1., 1., 1.};
  float light_specular[4] = {1., 1., 1., 1.};
  float light_position[4] = {2., 0., 0., 1.};

  glLightfv(GL_LIGHT0, GL_AMBIENT, light_ambient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, light_diffuse);
  glLightfv(GL_LIGHT0, GL_SPECULAR, light_specular);
  glLightfv(GL_LIGHT0, GL_POSITION, light_position);

  // Material parameters:
  float material_ambient[4] = {.2, .2, .2, 1.};
  float material_diffuse[4] = {1., 1., 1., 1.};
  float material_specular[4] = {1., 1., 1., 1.};
  float material_shininess = 50.;

  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, material_ambient);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, material_diffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, material_specular);
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, material_shininess);
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

void display()
{ 
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glMatrixMode(GL_MODELVIEW);
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

  init_light();

  glLoadIdentity();
  gluLookAt(2.5, 2.5, 2.5, 0., 0., 0., 0., 1., 0.);

  draw_cartesian_coordinates();

  glColor3f(0.7, 0.7, 1.);

  glBegin(GL_QUADS);
    glVertex3f(1., 0., -1.);
    glVertex3f(-1., 0., -1.);
    glVertex3f(-1., 0., 1.);
    glVertex3f(1., 0., 1.);
  glEnd();

  glutSwapBuffers(); 
  glutPostRedisplay();
}

void keyboard(unsigned char key, int x, int y)
{
  switch(key) {
    case 27:
      exit (0);
      break;
    default:
      break;
  }
}

void reshape(int width, int height)
{  
  glViewport(0,0, width, height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(45, (float)width / (float)height, 0.1, 100);
  glMatrixMode(GL_MODELVIEW);
}  

//! \brief Main program.
int main(int argc, char **argv)
{
  GLenum code;

  // Need create valid OpenGL rendering context before initialise GLEW
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_RGB | GLUT_DEPTH | GLUT_DOUBLE);
  glutInitWindowSize(700, 500);
  glutCreateWindow("Mesh-Deformers Test");

  glutReshapeFunc(reshape);
  glutDisplayFunc(display);
  glutKeyboardFunc(keyboard);

  init();
  code = glewInit();
  if(code != GLEW_OK) {
    fprintf(stderr, "Impossible d'initialiser GLEW : %s\n",
        glewGetErrorString(code));
    exit(0);
  }

  if(!glewIsSupported("GL_ARB_vertex_shader")
      || !glewIsSupported("GL_ARB_fragment_shader")) {
    fprintf (stderr, "Shaders non supportés.\n");
    exit(0);
  }

  glutMainLoop();
  return 0;
}

