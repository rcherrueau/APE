/*!
 * \file    main.c
 * \brief   testing shader_compiling with one vertex shader.
 *
 * Program uses GLEW to provide mechanisms for determining if GL_ARB_**_shader
 * extensions are supported on platform. This uses glut for window system.
 *
 * \sa      http://glew.sourceforge.net/
 *
 * \author  Ronan-Alexandre Cherrueau ronancherrueau{at}gmail{dot}com
 * \date    last modified 06/06/2011
 * \date    first release 20/05/2011
 */

#include <stdlib.h>
#include <stdio.h>

#include <GL/glew.h>
#include <GL/glut.h>

#include "shader_compiling.h"
#include "shader_samples.h"

double a=0;
float light_pos[4] = {0,0.7,2,1};

void init()
{
	glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
}

void display()
{ 
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glMatrixMode(GL_MODELVIEW);

  // Put Cam and Sphere
  glLoadIdentity();
  gluLookAt(0., 2., 5., 0., 0., 0., 0., 1., 0.);
  glutSolidTeapot(1);

  // Rotate and put light
  glRotated(a, 0, 1, 0);
  glLightfv(GL_LIGHT0, GL_POSITION, light_pos);
  glTranslatef(0,0.7,2.5);
  glutSolidSphere(0.1, 50, 50);

  a += 0.1;

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
  glViewport(0,0,width,height);
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
  glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE);
  glutInitWindowSize(700, 500);
  glutCreateWindow("GLEW Test");

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

  // trivial_shader();
  gouraud_shader();
  // cel_shader();
  // uniform_shader();

  glutMainLoop();
  return 0;
}

