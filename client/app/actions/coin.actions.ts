import { Dispatch } from 'redux';
import { CoinService } from '../services';
import { coinConstants } from '../constants';
import { alertActions } from './alert.actions';

interface ICoinActions
{
    getAssets: (dispatch: Dispatch<any>) => void,
    getCurrentExchangeRate: (baseAsset: string, quoteAsset: string) => ((dispatch: Dispatch<any>) => void),
    getAllTimePeriods: (dispatch: Dispatch<any>) => void
}

export const coinActions: ICoinActions =
{
    getAssets: getAssets,
    getCurrentExchangeRate: getCurrentExchangeRate,
    getAllTimePeriods: getAllTimePeriods,
};

function getAssets(): (dispatch: Dispatch<any>) => void
{
    return async (dispatch) =>
    {
        // Set that the assets are being retrieved
        dispatch({
            type: coinConstants.GET_ASSETS_REQUEST
        });

        try
        {
            const assets = await CoinService.getAssets();

            dispatch({
                type: coinConstants.GET_ASSETS_SUCCESS,
                assets: assets
            });

            dispatch(alertActions.success("Get Assets Success"));
        }
        catch(error)
        {
            dispatch({
                type: coinConstants.GET_ASSETS_ERROR,
                error: error
            });

            dispatch(alertActions.error("Get Assets Error"));
        }
    }
}

function getCurrentExchangeRate(baseAsset: string, quoteAsset: string): (dispatch: Dispatch<any>) => void
{
    return async (dispatch) =>
    {
        dispatch({
            type: coinConstants.GET_CURRENT_EXCHANGERATE_REQUEST,
            base: baseAsset,
            quoteAsset: quoteAsset
        });

        try
        {
            const exchangeRate = await CoinService.getCurrentExchangeRate(baseAsset, quoteAsset);

            dispatch({
                type: coinConstants.GET_CURRENT_EXCHANGERATE_SUCCESS,
                exchangeRate: exchangeRate
            });

            dispatch(alertActions.success("Get Current Exchange Rate Success"));
        }
        catch(error)
        {
            dispatch({
                type: coinConstants.GET_CURRENT_EXCHANGERATE_ERROR,
                error: error
            });

            dispatch(alertActions.error("Get Current Exchange Rate Error"));
        }
    }
}

function getAllTimePeriods(): (dispatch: Dispatch<any>) => void
{
    return async (dispatch) =>
    {
        dispatch({
            type: coinConstants.GET_TIME_PERIODS_REQUEST
        });

        try
        {
            const timePeriods = await CoinService.getAllTimePeriods();

            dispatch({
                type: coinConstants.GET_TIME_PERIODS_SUCCESS,
                timePeriods: timePeriods
            });

            dispatch(alertActions.success("Get Time Periods Success"));
        }
        catch(error)
        {
            dispatch({
                type: coinConstants.GET_TIME_PERIODS_ERROR,
                error: error
            });

            dispatch(alertActions.error("Get Time Periods Error"));
        }
    }
}