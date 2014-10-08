#include <stdlib.h>
#include <string>
#include <iostream>
#include <fstream>

#include <google/protobuf/stubs/common.h>
#include <google/protobuf/message.h>

using namespace std;

namespace haskell {

int readProtobuf(char const *file, ::google::protobuf::Message &message) {
  fstream input(file, ios::in | ios::binary);
  if (!input) {
    return -2;
  } else if (!message.ParseFromIstream(&input)) {
    return -1;
  }
  return 0;
}

int writeProtobuf(char const *file, ::google::protobuf::Message &message) {
  fstream output(file, ios::out | ios::trunc | ios::binary);
  if (!message.SerializeToOstream(&output)) {
    return -1;
  }
  return 0;
}

}
