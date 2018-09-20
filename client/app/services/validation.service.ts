import { FormErrors } from "redux-form";
import { User } from '../models';

export const required = (value: any) => 
{
    if (value === undefined)
    {
        return 'Required';
    }
}

export const validateUsername = (value: any) =>
{
    if (value.length < 8)
    {
        return 'Must be at least 8 characters';
    }
}

export const validatePassword = (value: any) =>
{
    if (value.length < 10)
    {
        return 'Must be at least 10 characters';
    }
}

export const validateEmail = (value: any) =>
{
    if (!/^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}$/i.test(value))
    {
        return 'Invalid Email'
    }
}

export const validatePhone = (value: any) =>
{
    if (value.length < 10 || !/^[\+]?[(]?[0-9]{3}[)]?[-\s\.]?[0-9]{3}[-\s\.]?[0-9]{4,6}$/im.test(value))
    {
        return 'Invalid Phone Number';
    }
}