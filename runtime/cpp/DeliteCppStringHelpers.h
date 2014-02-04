#ifndef __DELITE_CPP_STRING_HELPERS_H__
#define __DELITE_CPP_STRING_HELPERS_H__

#define charAsString char

#include "cppDeliteArray.h"
#include <assert.h>
#include <stdio.h>

charAsString* magic_trim_function(charAsString* str);
cppDeliteArray< charAsString * >* magic_split_function(charAsString* str, charAsString* pattern);
double magic_atod(charAsString* str);

#endif // __DELITE_CPP_STRING_HELPERS_H__
