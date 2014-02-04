#include "cppHelpers.h"
template <typename T>
vector<T>* vectorize(cppDeliteArray<T> *arr)
{
    std::vector<T> *v = new vector<T>(arr->data, arr->data + arr->length);
    return v;
}

// C++11 required! Use -std=c++0x as a compile-time option, else
// compilation will fail!
template <typename T>
std::vector<size_t> index_sort(std::vector<T> const& values) {
    std::vector<size_t> indices(values.size());
    std::iota(begin(indices), end(indices), static_cast<size_t>(0));

    std::sort(begin(indices), end(indices),[&](size_t a, size_t b) { return values[a] < values[b]; });
    return indices;
}


