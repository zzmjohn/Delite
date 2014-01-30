#ifndef _CPP_DELITEARRAYNUMA_H_
#define _CPP_DELITEARRAYNUMA_H_

#include <stdlib.h>
#include <string.h>

#include "DeliteCpp.h"

template <class T>
class cppDeliteArrayNuma {
public:
    T **wrapper;
    int length;
    int numGhostCells; // constant for all internal arrays

    // Constructor
    cppDeliteArrayNuma(int _length, int _numGhostCells) {
        length = _length;
        numGhostCells = _numGhostCells;
        wrapper = (T **)malloc(config->numSockets*sizeof(T*));
    }

    T apply(int idx) {
      return applyAt(0, idx);
    }

    void update(int idx, T value) {
      updateAt(0, idx, value);
    }

    int internalLength() {
      return std::min(length, length/config->numSockets+numGhostCells);
    }

    // Additional functions
    // void copy(int srcOffset, cppDeliteArray<T> *dest, int destOffset, int length) {
    //   memcpy(dest->data + destOffset, data + srcOffset, sizeof(T) * length);
    // }

    // cppDeliteArray<T> *take(int n) {
    //   cppDeliteArray<T> *result = new cppDeliteArray<T>(n);
    //   memcpy(result->data, data, sizeof(T) * n);
    //   return result;
    // }

    // NUMA-specific functionality
    void allocInternal(int tid) {
      // TODO: this ghostCell stuff is half-baked right now
      wrapper[config->threadToSocket(tid)] = (T*)malloc(internalLength()*sizeof(T));
    }

    T applyAt(int tid, int i) {
      return wrapper[config->threadToSocket(tid)][i];
    }

    void updateAt(int tid, int i, T value) {
      wrapper[config->threadToSocket(tid)][i] = value;
    }

    void combineAverage() {
      // calls to this should only be generated on type T <: Numeric, as checked inside the Delite compiler
      T* avg = (T*)malloc(numGhostCells*sizeof(T));
      for (int s = 0; s < config->numSockets; s++) {
        // currently only ghosting "to the right"
        int start = internalLength() - numGhostCells;
        for (int i = start; i < numGhostCells+start; i++) {
          avg[i-start] += wrapper[s][i];
        }
      }

      for (int i = 0; i < numGhostCells; i++) {
        avg[i] = avg[i] / config->numSockets;
      }

      for (int s = 0; s < config->numSockets; s++) {
        int start = internalLength() - numGhostCells;
        for (int i = start; i < numGhostCells+start; i++) {
          wrapper[s][i] = avg[i-start];
        }
      }
    }

    // --
    void release(void);
};

#endif


/*

abstract class NumaDeliteArrayDouble(val length: Int) extends DeliteArrayDouble {
  val wrapper: Array[Array[Double]] = new Array[Array[Double]](Config.numSockets)

  // the following fields are only relevant for cluster + NUMA, which we don't support yet
  def readAt(i: Int): T = ???
  var id: String = ???
  var offsets: Array[Int] = ???
  var offset: Int = ???
  def data: Array[Double] = ???

  def apply(i: Int): Int = applyAt(0,i)
  def update(i: Int, x: Double) = updateAt(0,i,x)

  def take(n: Int): NumaDeliteArrayDouble = {
    val res = new NumaDeliteArrayDouble(n)
    copy(0, res, 0, n)
    res
  }
  def copy(srcPos: Int, dest: NumaDeliteArrayDouble, destPos: Int, len: Int) {
    for (i <- 0 until wrapper.length) {
      System.arraycopy(m.wrapper(i), srcPos, dest.wrapper(i), destPos, len)
    }
  }

  def tidToSocket(tid: Int) = tid % Config.numSockets

  def allocInternal(tid: Int, numGhostCells: Int) {
    // TODO: this ghostCell stuff is half-baked right now
    wrapper(tidToSocket(tid)) = new Array[Double](Math.max(length, length/Config.numSockets+numGhostCells))
  }

  def applyAt(tid: Int, i: Int): Int = wrapper(tidToSocket(tid)).apply(i)
  def updateAt(tid: Int, i: Int, x: Double) { wrapper(tidToSocket(tid)).update(i,x) }

  def combine(f: (Array[Double],Array[Double]) => Array[Double]) = ???
}

*/
