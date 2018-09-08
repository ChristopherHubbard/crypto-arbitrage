import * as React from 'react';

// Custom input component -- stateless component (handles re-rendering better if state isnt needed)
export const CustomInput = ({ input, label, type, meta: { error, touched } }: any) => {

    return (
        <div className="row-md">
            <label> {label} </label>

            <div>
                <input className="form-text-input primary-border"  type={type} {...input} placeholder={label}/>

                {touched && error && 
                <span className="error"> {error} </span>}
            </div>
        </div>
    );
};