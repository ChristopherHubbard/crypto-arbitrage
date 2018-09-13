import { coinConstants } from '../constants';
import { IAction, ExchangeRateState } from '../models';

function exchangeRate(state: ExchangeRateState = {}, action: IAction): ExchangeRateState
{
    switch(action.type)
    {
        case coinConstants.GET_CURRENT_EXCHANGERATE_REQUEST:
            return {
                loadingRate: true,
                baseAsset: action.baseAsset,
                quoteAsset: action.quoteAsset
            }
        case coinConstants.GET_CURRENT_EXCHANGERATE_SUCCESS:
            return {
                loadedRate: true,
                exchangeRate: action.exchangeRate
            }
        case coinConstants.GET_CURRENT_EXCHANGERATE_ERROR:
            return {};
        default:
            return state;
    }
}