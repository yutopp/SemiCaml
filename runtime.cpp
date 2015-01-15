#include <iostream>
#include <cstdint>

extern "C" {
    auto _semi_caml_entry() -> void;
}

int main()
{
    // jump intp SemiCaml world
    _semi_caml_entry();
}

extern "C"
{
    auto _semi_caml_print_int( std::int32_t const v )
        -> void
    {
        std::cout << v;
    }
}
