import { userConstants } from '../constants';
import { User, AuthenticationState, IAction } from '../models';

const user: User = JSON.parse(JSON.stringify(localStorage.getItem('user')));
const initialState: AuthenticationState = user ? { loggedIn: true, user: user } : {};

export function authentication(state: AuthenticationState = initialState, action: IAction): AuthenticationState
{
    // Go through possible states for authentication
    switch (action.type)
    {
        case userConstants.LOGIN_REQUEST:
            return <AuthenticationState> {
                loggingIn: true,
                user: action.user
            };
        case userConstants.LOGIN_SUCCESS:
            return <AuthenticationState> {
                loggedIn: true,
                user: action.user
            };
        case userConstants.LOGIN_FAILURE:
            return <AuthenticationState> {};
        case userConstants.LOGOUT_SUCCESS:
            return <AuthenticationState> {};
        default:
            return state;
    }
}

