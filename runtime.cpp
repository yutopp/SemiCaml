#include <iostream>
#include <cstdint>

extern "C" {
    auto _semi_caml_entry() -> void;
}

int main()
{
    // jump into SemiCaml world
    _semi_caml_entry();
}

extern "C"
{
    auto _semi_caml_print_int( std::int32_t const* const v )
        -> void
    {
        std::cout << *v;
    }

    // TODO: memory management
    auto _semi_caml_new_int32( std::int32_t const v )
        ->  std::int32_t*
    {
        return new std::int32_t( v );
    }
}
