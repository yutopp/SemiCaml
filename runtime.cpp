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

    auto _semi_caml_print_bool( bool const* const v )
        -> void*
    {
        std::cout << (*v ? "true" : "false");

        return nullptr;
    }

    auto _semi_caml_print_float( float const* const v )
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
    using holder_t = void*;

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

    auto _semi_caml_new_bool( bool const v )
        -> bool*
    {
        return new bool( v );
    }

    auto _semi_caml_new_array( std::int32_t const length, void* const init_value )
        -> holder_t*
    {
        auto const a_length = length + 1;
        auto array = new holder_t[a_length];
        array[0] = _semi_caml_new_int32( a_length );
        for (auto i=1; i<a_length; ++i) {
            array[i] = init_value;
        }

        return array;
    }

    auto _semi_caml_ref_array_element( holder_t* arr, std::int32_t const index )
        -> holder_t
    {
        std::int32_t const length = *static_cast<int*>( arr[0] );
        if ( index < 0 || index >= length ) {
            std::cout << "Out of range: " << index << std::endl;
        }

        return arr[index+1];
    }

    auto _semi_caml_assign_array_element( holder_t* arr, std::int32_t const index, void* v )
        -> void
    {
        std::int32_t const length = *static_cast<int*>( arr[0] );
        if ( index < 0 || index >= length ) {
            std::cout << "Out of range: " << index << std::endl;
        }

        arr[index+1] = v;
    }

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
