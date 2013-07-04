/*!
 * \brief   Php Extension Writing with Zend to wrapp c++ classes.
 *
 * This file is the implementation of Wrapping C++ Classes in a PHP Extension by
 * Paul Osman.
 *
 * \see     http://devzone.zend.com/article/4486
 */
#include "php_vehicles.h"
#include "car.h"

zend_class_entry *car_ce;
zend_object_handlers car_object_handlers;

struct car_object {
  zend_object std;
  Car *car;
};

static function_entry car_methods[] = {
  PHP_ME(Car, __construct, NULL, ZEND_ACC_PUBLIC | ZEND_ACC_CTOR)
    PHP_ME(Car, shift, NULL, ZEND_ACC_PUBLIC)
    PHP_ME(Car, accelerate, NULL, ZEND_ACC_PUBLIC)
    PHP_ME(Car, brake, NULL, ZEND_ACC_PUBLIC)
    PHP_ME(Car, speed, NULL, ZEND_ACC_PUBLIC)
    PHP_ME(Car, gear, NULL, ZEND_ACC_PUBLIC)
    {NULL, NULL, NULL}
};

zend_module_entry vehicles_module_entry = {
#if ZEND_MODULE_API_NO >= 20010901
  STANDARD_MODULE_HEADER,
#endif
  PHP_VEHICLES_EXTNAME,
  NULL,                   //!< Functions
  PHP_MINIT(vehicles),
  NULL,                   //!< MSHUTDOWN
  NULL,                   //!< RINIT
  NULL,                   //!< RSHUTDOWN
  NULL,                   //!< MINFO
#if ZEND_MODULE_API_NO >= 20010901
  PHP_VEHICLES_EXTNAME,
#endif
  STANDARD_MODULE_PROPERTIES
};

#ifdef COMPILE_DL_VEHICLES
ZEND_GET_MODULE(vehicles)
#endif

void car_free_storage(void *object TSRMLS_DC)
{
  car_object *obj = (car_object*) object;
  delete obj->car;

  zend_hash_destroy(obj->std.properties);
  FREE_HASHTABLE(obj->std.properties);

  efree(obj);
}

zend_object_value car_create_handler(zend_class_entry *type TSRMLS_DC)
{
  zval *tmp;
  zend_object_value retval;

  car_object *obj = (car_object*)emalloc(sizeof(car_object));
  memset(obj, 0, sizeof(car_object));
  obj->std.ce = type;

  ALLOC_HASHTABLE(obj->std.properties);
  zend_hash_init(obj->std.properties, 0, NULL, ZVAL_PTR_DTOR, 0);
  zend_hash_copy(obj->std.properties, &type->default_properties,
      (copy_ctor_func_t)zval_add_ref, (void *)&tmp, sizeof(zval *));

  retval.handle = zend_objects_store_put(obj, NULL, car_free_storage,
      NULL TSRMLS_CC);
  retval.handlers = &car_object_handlers;

  return retval;
}

PHP_MINIT_FUNCTION(vehicles)
{
  zend_class_entry ce;
  INIT_CLASS_ENTRY(ce, "Car", car_methods);
  car_ce = zend_register_internal_class(&ce TSRMLS_CC);
  car_ce->create_object = car_create_handler;
  memcpy(&car_object_handlers, zend_get_std_object_handlers(),
      sizeof(zend_object_handlers));
  car_object_handlers.clone_obj = NULL;
  return SUCCESS;
}

PHP_METHOD(Car, __construct)
{
  long max_gear;
  Car *car = NULL;
  zval *object = getThis();

  if(zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC, "l", &max_gear)
      == FAILURE) {
    RETURN_NULL();
  }

  car = new Car(max_gear);
  car_object *obj =(car_object*) zend_object_store_get_object(
      object TSRMLS_CC);
  obj->car = car;
}

PHP_METHOD(Car, shift)
{
  long gear;
  Car *car = NULL;
  car_object *obj = (car_object *)zend_object_store_get_object(
      getThis() TSRMLS_CC);
  car = obj->car;

  if(zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC, "l", &gear)
      == FAILURE) {
    RETURN_NULL();
  }

  if(car != NULL) {
    car->shift(gear);
  }

  RETURN_TRUE;
}

PHP_METHOD(Car, accelerate)
{
  Car *car = NULL;
  car_object *obj = (car_object *)zend_object_store_get_object(
      getThis() TSRMLS_CC);
  car = obj->car;
  if(car != NULL) {
    car->accelerate();
  }

  RETURN_TRUE;
}

PHP_METHOD(Car, brake)
{
  Car *car = NULL;
  car_object *obj = (car_object *)zend_object_store_get_object(
      getThis() TSRMLS_CC);
  car = obj->car;
  if(car != NULL) {
    car->brake();
  }

  RETURN_TRUE;
}

PHP_METHOD(Car, speed)
{
  Car *car = NULL;
  car_object *obj = (car_object *)zend_object_store_get_object(
      getThis() TSRMLS_CC);
  car = obj->car;
  if(car != NULL) {
    RETURN_LONG(car->speed());
  }

  RETURN_NULL();
}

PHP_METHOD(Car, gear)
{
  Car *car = NULL;
  car_object *obj = (car_object *)zend_object_store_get_object(
      getThis() TSRMLS_CC);
  car = obj->car;
  if(car != NULL) {
    RETURN_LONG(car->gear());
  }

  RETURN_NULL();
}

