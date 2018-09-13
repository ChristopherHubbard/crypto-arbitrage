import axios, { AxiosResponse, AxiosError, AxiosRequestConfig } from 'axios';
import { User } from '../models';
import Config from '../config';

// Abstract since totally static class
export abstract class UserService
{
    // Static async method to register a user (Put in DB) given info (should be validated)
    public static async register(user: User): Promise<any>
    {
        // Options for the post to add the user -- type?
        const requestOptions: any =
        {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: user
        };

        try
        {
            // Await the response -- shouldn't be any returned data for the post
            return await axios.post(`${Config.apiUrl}/users/register`, requestOptions);
        }
        catch(error)
        {
            // Log any error
            console.error(error);
        }
    }

    // Static async method to login with user and password -- should probably have better return type here
    public static async login(user: User): Promise<any>
    {
        // Create the options for the request -- type?
        const requestOptions: any =
        {
            method: 'GET',
            headers: { 'Content-Type': 'application/json' },
            params: user
        };

        // Try catch for the new Async-Await structure -- hopefully works
        try
        {
            // Await the response
            const response: AxiosResponse = await axios.get(`${Config.apiUrl}/users/authenticate`, requestOptions);

            // Check for the response token -- this proves authentication
            const user = await response.data;
            if (user.token)
            {
                // Add the user to the localStorage
                localStorage.setItem('user', JSON.stringify(user));
            }

            // Return the user -- not necessarily authenticated
            return await user;
        }
        catch(error)
        {
            // Log the error
            console.log(error);
        }
    }

    public static logout(): void
    {
        // Remove the user from the localStorage
        localStorage.removeItem('user');
    }
}