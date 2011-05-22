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
 * \date    last modified 20/05/2011
 * \date    first release 20/05/2011
 */

#include <stdlib.h>
#include <stdio.h>

#include <GL/glew.h>
#include <GL/glut.h>

#include "shader_compiling.h"

shader_resources sc;
double a=0;

void init()
{
	glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
}

void display()
{ 
  int light_pos[4] = {0,0,2,1};
  int mat_spec [4] = {1,1,1,1};

  glMaterialiv(GL_FRONT_AND_BACK, GL_SPECULAR, mat_spec);
  glMateriali(GL_FRONT_AND_BACK, GL_SHININESS, 100);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glMatrixMode(GL_MODELVIEW);

  // Put Cam and Sphere
  glLoadIdentity();
  gluLookAt(0, 5, 6, 0, 0, 0, 0, 1, 0);
  glutSolidSphere(1, 50, 50);

  // Rotate and put light
  glRotated(a, 0, 1, 0);
  glLightiv(GL_LIGHT0, GL_POSITION, light_pos);
  glTranslatef(0,0,2.5);
  glutSolidSphere(0.1, 50, 50);

  a += 0.05;

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
  glutInitWindowSize(500, 500);
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

  sc.vertex_shader = load_shader(GL_VERTEX_SHADER,
      "../samples/vertex_shaders/trivial_vertex_shader.vert");
  if(sc.vertex_shader == 0) {
    exit(0);
  }
  
  /*
  // Testing with one vertex shader.
  sc.program = make_program_from_one(sc.vertex_shader);
  if(sc.program == 0) {
    exit(0);
  }
  //*/

  //*
  // Testing with vertex shader ans pixel shader
  sc.fragment_shader = load_shader(GL_FRAGMENT_SHADER,
      "../samples/fragment_shaders/trivial_fragment_shader.frag");
  if(sc.fragment_shader == 0) {
    exit(0);
  }

  sc.program = make_program_from_two(sc.vertex_shader, sc.fragment_shader);
  if(sc.program == 0) {
    exit(0);
  }
  //*/

  glUseProgram(sc.program);
  glutMainLoop();
  return 0;
}

