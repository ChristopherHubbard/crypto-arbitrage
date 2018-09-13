import axios, { AxiosResponse, AxiosError, AxiosRequestConfig } from 'axios';
import config from '../config';

export abstract class CoinService
{
    private static coinAPIKey: string = "2AF81466-C9D7-4F6C-8AC0-B9451D052BB3";

    public static async getAssets(): Promise<any>
    {
        const requestOptions: any =
        {
            method: "GET",
            headers: { "X-CoinAPI-Key": CoinService.coinAPIKey }
        };

        // Send the request to CoinAPI
        try
        {
            return await axios.get(`${config.coinAPIUrl}/v1/assets`, requestOptions);
        }
        catch(error)
        {
            console.error(error);
        }
    }

    // Make sure that this works for extended combinations
    public static async getCurrentExchangeRate(baseAsset: string, quoteAsset: string): Promise<any>
    {
        const requestOptions: any =
        {
            method: "GET",
            headers: { "X-CoinAPI-Key": CoinService.coinAPIKey }
        };

        // Send the request
        try
        {
            return await axios.get(`${config.coinAPIUrl}/v1/exchangerate/${baseAsset}/${quoteAsset}`, requestOptions);
        }
        catch(error)
        {
            console.error(error);
        }
    }

    public static async getAllTimePeriods(): Promise<any>
    {
        const requestOptions: any =
        {
            method: "GET",
            headers: { "X-CoinAPI-Key": CoinService.coinAPIKey }
        };

        // Send the request
        try
        {
            return await axios.get(`${config.coinAPIKey}/v1/ohlcv/periods`, requestOptions);
        }
        catch(error)
        {
            console.error(error);
        }
    }

    // Get the historical data -- this is for a particular exchange tho?
    public static async getOHCVLData(): Promise<any>
    {
        
    }
}