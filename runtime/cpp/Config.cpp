#ifndef _DELITE_CONFIG_
#define _DELITE_CONFIG_

#include <iostream>
#include <jni.h>
#include <numa.h>
#include "Config.h"

Config* config = 0;

void initializeConfig(int numThreads) {
    config = new Config(numThreads);
    if (numa_available() >= 0) config->numSockets = numa_num_configured_nodes();
    //printf("numThreads: %d\n", config->numThreads);
    //printf("numSockets: %d\n", config->numSockets);
}

extern "C" JNIEXPORT void JNICALL Java_ppl_delite_runtime_executor_AccExecutionThread_initializeThread(JNIEnv* env, jobject obj, jint threadId, jint numThreads);

JNIEXPORT void JNICALL Java_ppl_delite_runtime_executor_AccExecutionThread_initializeThread(JNIEnv* env, jobject obj, jint threadId, jint numThreads) {
    if (!config) initializeConfig(numThreads);
    if (numa_available() >= 0) {
        int socketId = config->threadToSocket(threadId);
        if (socketId < numa_num_configured_nodes()) {
            printf("binding thread %d to socket %d\n", threadId, socketId);
            bitmask* nodemask = numa_allocate_nodemask();
            numa_bitmask_setbit(nodemask, socketId);
            numa_bind(nodemask);
        }
    }
}

#endif

