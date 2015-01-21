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
        -> void*
    {
        std::cout << *v;

        return nullptr;
    }

    auto _semi_caml_print_newline( void* )
        -> void*
    {
        std::cout << std::endl;

        return nullptr;
    }


    // TODO: memory management
    auto _semi_caml_new_int32( std::int32_t const v )
        -> std::int32_t*
    {
        return new std::int32_t( v );
    }

    auto _semi_caml_new_float( float const v )
        -> float*
    {
        return new float( v );
    }

    auto _semi_caml_new_bool( std::int8_t const v )
        -> std::int8_t*
    {
        return new std::int8_t( v );
    }

    using holder_t = void*;

    struct closure_bag_t
    {
        void* fp;
        holder_t* captured;
        holder_t* args;
    };

    auto _semi_caml_new_value_holder_list( std::int32_t const length )
        -> holder_t*
    {
        return new holder_t[length];
    }

    auto _semi_caml_new_closure_bag( void* const fp, std::int32_t const c_length )
        -> closure_bag_t*
    {
        auto captured_holder = _semi_caml_new_value_holder_list( c_length );

        auto b = closure_bag_t{
            fp,
            captured_holder,
            nullptr
        };

        return new closure_bag_t( b );
    }
}
