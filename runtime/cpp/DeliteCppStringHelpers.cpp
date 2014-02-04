#include "cppStringHelpers.h"
#include <iostream>
charAsString* magic_trim_function(charAsString *str)
{

    return str;
}

cppDeliteArray< charAsString * >* magic_split_function(charAsString *str, charAsString *pattern)
{
    cppDeliteArray < charAsString*> *retVal = (cppDeliteArray<charAsString*>*)malloc(sizeof(cppDeliteArray<charAsString*>));

    std::string s(str);
    std::vector<std::string> elems;
    std::stringstream ss(s);
    std::string item;
    char delim = ' ';
    while(getline(ss, item, delim)) {
        elems.push_back(item);
    }
    char **res = (char**)malloc(elems.size()*sizeof(char*));
    for (int i = 0; i<elems.size(); i++) {
        res[i] = (char*)elems[i].c_str();
//        std::cout << "res[" << i << "] = " << res[i] << std::endl;
    }
    retVal->data = (char**)res;
    retVal->length = elems.size();
    return retVal;
}

double magic_atod(charAsString *str)
{
    double d = strtod((const charAsString*)str, 0);
    return d;
}

bool magic_startsWith_function(charAsString *str, charAsString *substr)
{
   int i = 0;
   int substrLen = strlen(substr);
   assert(substrLen <= strlen(str));
   for (i = 0; i < substrLen; i++) {
       if (str[i] != substr[i]) {
           return false;
       }
   }
   return true;
}
