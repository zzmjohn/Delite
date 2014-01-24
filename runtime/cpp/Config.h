#ifndef _DELITE_CONFIG_H_
#define _DELITE_CONFIG_H_

class Config {
public:
    int numThreads;
    int numSockets;

    Config(int _numThreads) {
      numThreads = _numThreads;
      numSockets = 1;
    }

    Config(int _numThreads, int _numSockets) {
      numThreads = _numThreads;
      numSockets = _numSockets;
    }

    //current strategy is to distribute threads across sockets as evenly as possible
    int threadToSocket(int threadId) {
        float threadsPerSocket = (float)numThreads / numSockets;
        int socketId = threadId / threadsPerSocket;
        return socketId;
    }
};

#endif

