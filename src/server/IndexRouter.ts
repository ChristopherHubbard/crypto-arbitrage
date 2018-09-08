import { NextFunction, Request, Response, Router } from "express";

// Import base route class
import { CustomRouter } from "./CustomRouter";

const path: any = require("path");

// Temp data store
let users: any = [];

// Defines the routes used at the index of the application
export class IndexRouter extends CustomRouter
{
    // Likely no need for a constructor -- only need to define the routes now!!

    // Implement the route creating method
    protected CreateRoutes() : void
    {
        //Add home page route
        this.router.get('/', function(req: Request, res: Response, next: NextFunction) 
        {
            // This is one of the sections that needs to be changed for react v angular v vue -- this sends the homepage to the front
            res.sendFile(path.join(__dirname, '/index.html'));
        });

        this.router.post('/users/register', function(req : Request, res : Response, next : NextFunction): void
        {
            users.push(req.body.body);
        });
    }
}