import { alertConstants } from '../constants/alert.constants';

// Define interface types
interface IAlertActions
{
    success: (message: string) => IAction
    error: (message: string) => IAction
    clear: (message: string) => IAction
}

// This IAction interface might want to be moved over to some interfaces folder
interface IAction
{
    type: string
}

interface IMessageAction extends IAction
{
    message: string
}

// Define exported object
export const alertActions: IAlertActions =
{
    success: success,
    error: error,
    clear: clear
};

// Define redux action functions
function success(message: string): IMessageAction
{
    return {
        type: alertConstants.SUCCESS,
        message: message
    };
}

function error(message: string): IMessageAction
{
    return {
        type: alertConstants.ERROR,
        message: message
    };
}

function clear(message: string): IMessageAction
{
    return {
        type: alertConstants.CLEAR,
        message: message
    }
}
