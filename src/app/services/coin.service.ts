import axios, { AxiosResponse, AxiosError, AxiosRequestConfig } from 'axios';
import config from '../config';

export abstract class CoinService
{
    private static coinAPIKey: string = "2AF81466-C9D7-4F6C-8AC0-B9451D052BB3";

    // Request options will always be the same -- cant do post requests
    private static requestOptions: any =
    {
        method: "GET",
        headers: { "X-CoinAPI-Key": CoinService.coinAPIKey }
    }

    public static async getAssets(): Promise<any>
    {
        // Send the request to CoinAPI
        try
        {
            return await axios.get(`${config.coinAPIUrl}/v1/assets`, CoinService.requestOptions);
        }
        catch(error)
        {
            console.error(error);
        }
    }

    // Make sure that this works for extended combinations
    public static async getCurrentExchangeRate(baseAsset: string, quoteAsset: string): Promise<any>
    {
        // Send the request
        try
        {
            return await axios.get(`${config.coinAPIUrl}/v1/exchangerate/${baseAsset}/${quoteAsset}`, CoinService.requestOptions);
        }
        catch(error)
        {
            console.error(error);
        }
    }

    public static async getAllTimePeriods(): Promise<any>
    {
        // Send the request
        try
        {
            return await axios.get(`${config.coinAPIKey}/v1/ohlcv/periods`, CoinService.requestOptions);
        }
        catch(error)
        {
            console.error(error);
        }
    }

    // Get the historical data -- this is for a particular exchange -- might have to be changed to reflect THIS exchange
    public static async getHistoricOHCVLData(baseAsset: string, quoteAsset: string, period_id: string, time_start: string): Promise<any>
    {
        // This is the default exchange that data will be sent through
        const defaultExchange: string = "BITSTAMP_SPOT";
        try
        {
            return await axios
                .get(`${config.coinAPIKey}/v1/ohcvl/BITSTAMP_SPOT_${baseAsset}_${quoteAsset}
                        /history?period_id=${period_id}&time_start=${time_start}`);
        }
        catch(error)
        {
            console.error(error);
        }
    }

    // Gets data in latest data first -- These API methods likely wont work for assets that arent in arbitrage yet (ETH-XRP etc)
    public static async getLatestOHCVLDatta(baseAsset: string, quoteAsset: string, period_id: string): Promise<any>
    {
        const defaultExchange: string = "BITSTAMP_SPOT";
        try
        {
            return await axios
                .get(`${config.coinAPIKey}/v1/ohlcv/${defaultExchange}_${baseAsset}_${quoteAsset}
                        /latest?period_id=${period_id}`);
        }
        catch(error)
        {
            console.error(error);
        }
    }
}