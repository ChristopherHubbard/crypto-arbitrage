import { User } from './user.model';

export interface AuthenticationState
{
    loggedIn?: boolean,
    loggingIn?: boolean,
    user?: User
}

export interface RegistrationState
{
    registering?: boolean,
    registered?: boolean
}

export interface ExchangeRateState
{
    baseAsset?: string,
    quoteAsset?: string,
    exchangeRate?: any, // Change this
    loadingRate?: boolean,
    loadedRate?: boolean
}