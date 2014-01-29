#ifndef _CPP_DELITEARRAY_H_
#define _CPP_DELITEARRAY_H_

#include <stdlib.h>
#include <string.h>
#include <vector>
#include <algorithm>
#include <iostream>

template <class T>
class cppDeliteArray {
public:
    T *data;
    int length;

    // Constructor
    cppDeliteArray(int _length) {
        length = _length;
        data = (T *)malloc(length*sizeof(T));
    }

    cppDeliteArray(T *_data, int _length) {
        length = _length;
        data = _data;
    }

    T apply(int idx) {
        return data[idx];
    }

    void update(int idx, T value) {
        data[idx] = value;
    }

    // DeliteCoolection
    int size() {
        return length;
    }

    T dcApply(int idx) {
        return data[idx];
    }

    void dcUpdate(int idx, T value) {
        data[idx] = value;
    }
    
    // Additional functions
    void copy(int srcOffset, cppDeliteArray<T> *dest, int destOffset, int length) {
      memcpy(dest->data + destOffset, data + srcOffset, sizeof(T) * length);
    }

    cppDeliteArray<T> *arrayunion(cppDeliteArray<T> *rhs) {
      int newLength = length + rhs->length;
      cppDeliteArray<T> *result = new cppDeliteArray<T>(newLength);
      int acc = 0;
      for(int i=0; i<length; i++) {
        T elem = data[i];
        int j = 0;
        while(j < acc) {
          if(elem == result->data[j]) break;
          j += 1;
        }
        if(j == acc) result->data[acc++] = elem;
      }
      for(int i=0; i<rhs->length; i++) {
        T elem = rhs->data[i];
        int j = 0;
        while(j < acc) {
          if(elem == result->data[j]) break;
          j += 1;
        }
        if(j == acc) result->data[acc++] = elem;
      }
      result->length = acc-1;
      //TODO: Need to shrink the actual array size?
      return result;
    }

    cppDeliteArray<T> *intersect(cppDeliteArray<T> *rhs) {
      int newLength = max(length, rhs->length);
      cppDeliteArray<T> *result = new cppDeliteArray<T>(newLength);
      int acc = 0;
      for(int i=0; i<length; i++)
        for(int j=0; j<rhs->length; j++) 
          if(data[i] == rhs->data[j]) result->data[acc++] = data[i];
      result->length = acc-1;
      //TODO: Need to shrink the actual array size?
      return result;
    }
    
    cppDeliteArray<T> *take(int n) {
      cppDeliteArray<T> *result = new cppDeliteArray<T>(n);
      memcpy(result->data, data, sizeof(T) * n);
      return result;
    }

    std::vector<T>* vectorize()
    {
        std::vector<T> *v = new std::vector<T>(data, data + length);
        return v;
    }


    // C++11 required! Use -std=c++0x as a compile-time option, else
    // compilation will fail!
    cppDeliteArray<int>* index_sort()
    {
        std::vector<T> *values = this->vectorize();
        std::vector<int> indices(values->size());
        std::iota(begin(indices), end(indices), static_cast<int>(0));

        std::sort(begin(indices), end(indices),[&](int a, int b) { return values->at(a) < values->at(b); });
        
        cppDeliteArray<int> *result = new cppDeliteArray<int>(indices.size());
        for (int i=0; i<values->size(); i++) {
            result->data[i] = indices[i];
        }

        delete values;
        return result;
    }

    void print()
    {
        for(int i=0; i<length; i++) {
            std::cout << data[i] << " ";
        }
        std::cout << std::endl;
    }

    void release(void);
};

#endif
