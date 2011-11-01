/*!
 * \brief Php Extension Writing with Zend.
 * 
 * This file is the implementation of how to writing php extension by Sara
 * Golemon.
 *
 * \see   http://devzone.zend.com/article/1021
 * \see   http://devzone.zend.com/article/1022
 * \see   http://devzone.zend.com/article/1023
 */
#ifndef PHP_HELLO_H
#define PHP_HELLO_H

#define PHP_HELLO_WORLD_VERSION "0.1"
#define PHP_HELLO_WORLD_EXTNAME "hello"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "php.h"
#include "php_ini.h"

#ifdef ZTS
#include "TSRM.h"
#endif

ZEND_BEGIN_MODULE_GLOBALS(hello)
    long counter;
    zend_bool direction;
ZEND_END_MODULE_GLOBALS(hello)

#ifdef ZTS
#define HELLO_G(v) TSRMG(hello_globals_id, zend_hello_globals *, v)
#else
#define HELLO_G(v) (hello_globals.v)
#endif

PHP_MINIT_FUNCTION(hello);
PHP_MSHUTDOWN_FUNCTION(hello);
PHP_RINIT_FUNCTION(hello);

/*!
 * \brief   Returns string.
 *
 * \return  "hello world" string value.
 */
PHP_FUNCTION(hello_world);

/*!
 * \brief   Returns long.
 *
 * This function using global variable counter and print result of counter.
 * When hello php extension is loaded, the \c PHP_RINIT_FUNCTION(hello) is
 * called and a \a counter variable is initialized to 0.
 *
 * Moreover this function uses hello.direction boolean set in php.ini. If
 * hello.direction is set the value of \a counter will increase of 1, else
 * \a counter will decrease of 1.
 *
 * Value of hello.direction will getting when hello extension is loaded thanks
 * to \c PHP_MINIT_FUNCTION(hello).
 *
 * \return  value of global variable \a counter.
 */
PHP_FUNCTION(hello_long);

/*!
 * \brief   Returns double.
 *
 * \return  Pi double value.
 */
PHP_FUNCTION(hello_double);

/*!
 * \brief   Returns boolean.
 *
 * \return  \c TRUE boolean value.
 */
PHP_FUNCTION(hello_bool);

/*!
 * \brief   Returns NULL;
 *
 * \return  \c NULL value.
 */
PHP_FUNCTION(hello_null);

/*!
 * \brief   Prints "Hello <name>"
 *
 * \param   name  Name to print after "Hello" (zval).
 * \return  \c TRUE if all was great.
 */
PHP_FUNCTION(hello_greetme);

/*!
 * \brief   Adding long with double <tt>(a + b)</tt>
 *
 * \param   a   The first number (long).
 * \param   b   The next number (double).
 * \param   return_long Specifie if return addition to long (default \c FALSE).
 * \return  The result of addition.
 */
PHP_FUNCTION(hello_add);

/*!
 * \brief   Print type of variable give in parameter.
 *
 * \param   userval   The user value wich type will be printed (zval).
 * \return  \c TRUE if all was great.
 */ 
PHP_FUNCTION(hello_dump);

//! \brief   Create an array and adding subarray.
PHP_FUNCTION(hello_array);

/*!
 * \brief   Iterates and prints over an array.
 *
 * Function ietrate on array with following functions :
 *  \li \c zend_hash_internal_pointer_reset_ex() ;
 *  \li \c zend_hash_get_current_data_ex() ;
 *  \li \c zend_hash_move_forward_ex();
 *
 * \param   arr   The array to print (zval).
 * \return  \c TRUE if all was great.
 */
PHP_FUNCTION(hello_array_strings);

/*!
 * \brief   Iterates and prints over an array.
 *
 * Function ietrate on array with function \c zend_hash_apply(), that function
 * take an another function in parameter to say what do with each elements
 * of array. That parameter function is \c php_hello_array_walk().
 *
 * \param   zarray   The array to print (zval).
 * \return  \c TRUE if all was great.
 */
PHP_FUNCTION(hello_array_walk);

/*!
 * \brief   Looking for a specific value in array.
 *
 * Iterating through an array using foreach-style approach is a common task,
 * but often you'll be looking for a specific value in an array by index
 * number or by associative key. This function will return a value from an
 * array passed in the first parameter based on the offset or key specified in
 * the second parameter.
 *
 * \param   zarray  The array to looking for (zval).
 * \param   offset/key  Offset/key from which value interested (zval).
 * \return  Value at zarrey[offset] if offset exist (zval).
 */
PHP_FUNCTION(hello_array_value);

/*!
 * \brief   Gets value in \c $GLOBALS array.
 *
 * Gets and returns value in \c $GLOBALS array at offset \a varname.
 *
 \verbatim
 > ex in php: 

 $GLOBALS["toto"] = "toto";
 echo hello_get_global_var("toto"); // "toto"
 \endverbatim
 *
 * \param   varname   Name of expected value in \c $GLOBALS array (zval).
 * \return  Value at \c $GLOBALS[offset] if offset exist (zval).
 */
PHP_FUNCTION(hello_get_global_var);

extern zend_module_entry hello_module_entry;
#define phpext_hello_ptr &hello_module_entry

#endif

