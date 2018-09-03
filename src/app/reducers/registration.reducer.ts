import { userConstants } from '../constants/user.constants';
import { RegistrationState } from '../models/state.model';
import { IAction } from '../models/actions.model';

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