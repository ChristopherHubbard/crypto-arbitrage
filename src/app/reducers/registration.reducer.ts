import { userConstants } from '../constants';
import { RegistrationState, IAction } from '../models';

export function registration(state: RegistrationState = {}, action: IAction): RegistrationState
{
    switch (action.type)
    {
        case userConstants.REGISTER_REQUEST:
            return <RegistrationState> {
                registering: true
            };
        case userConstants.REGISTER_SUCCESS:
            return <RegistrationState> {
                registered: true
            };
        case userConstants.REGISTER_ERROR:
            return <RegistrationState> {
                registered: false
            };
        default:
            return state;
    }
}