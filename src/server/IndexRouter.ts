import { NextFunction, Request, Response, Router } from "express";

// Import base route class
import { CustomRouter } from "./CustomRouter";

// Import requests from mssql to make sql connections and transactions
import { ConnectionPool, config as ConnectionConfig, Request as SQLRequest, IProcedureResult, ConnectionError, RequestError, map as Map } from "mssql";

import { prop, Typegoose, ModelType, InstanceType } from 'typegoose';

import * as mongoose from 'mongoose';

const path: any = require("path");

// Get the sql Configuration in this environment -- Normally the WAYPOINT would be set by environment
const sqlConfig: ConnectionConfig = require("./config/sqlconfig.json")["DELL690"];

// Model for Typegoose -- wonder if there is a way to combine this with the SQL stuff
class SecurityTypeGoose extends Typegoose 
{
    @prop()
    Symbol?: string;

    @prop()
    Name?: string;

    @prop()
    CUSIP?: string;

    @prop()
    Weight?: string;
}

const SecurityModel: mongoose.Model<InstanceType<SecurityTypeGoose>> & SecurityTypeGoose & typeof SecurityTypeGoose = new SecurityTypeGoose().getModelForClass(SecurityTypeGoose,
{
    schemaOptions: { collection: "Securities" }
});

//Temporary Data store?
interface Security
{
    Symbol: string;
    Name: string;
    CUSIP: string;
    Weight: string;
}

interface ASPUser
{
    Id: string;
    Email: string;
    EmailConfirmed: string;
    PasswordHash: string;
    SecurityStamp: string;
    PhoneNumber: string;
    PhoneNumberConfirmed: string;
    TwoFactorEnabled: string;
    LockoutEndDateUtc: string;
    LockoutEnabled: string;
    AccessFailedCount: string;
    UserName: string;
}

// Defines the routes used at the index of the application
export class IndexRouter extends CustomRouter
{
    // Likely no need for a constructor -- only need to define the routes now!!

    // Implement the route creating method
    protected CreateRoutes() : void
    {
        //Add home page route
        this.router.get("/", function(req: Request, res: Response, next: NextFunction) 
        {
            // This is one of the sections that needs to be changed for react v angular v vue -- this sends the homepage to the front
            res.sendFile(path.join(__dirname, '/index.html'));
        });

        this.router.get("/SECURITIES", function(req : Request, res : Response, next : NextFunction): void
        {
            // Make a connection to the sql database
            let sqlConnection: ConnectionPool = new ConnectionPool(sqlConfig, function(error: ConnectionError): void
            {
                if (!error)
                {
                    // Create the SQL Request -- does this need the connectionPool??
                    let sqlRequest: SQLRequest = new SQLRequest(sqlConnection);
                    
                    // Execute this sProc
                    sqlRequest.execute<Security | ASPUser>("GetAllInstrumnents", function(error: RequestError, resultSets: IProcedureResult<Security | ASPUser> | undefined): void
                    {
                        if (!error && resultSets !== undefined)
                        {
                            // Return the result set
                            res.send(resultSets.recordsets[0]);
                        }
                        else
                        {
                            console.error("Error during SQL query: " + error.message);
                        }

                        // Close the SQL connection after the response has been sent and the error checking is done
                        sqlConnection.close();
                    });
                }
                else
                {
                    console.error("Error during SQL connection: " + error.message);
                }
            });
            
        });

        //Use post requests to update existing resources
        this.router.post("/SECURITIES", function(req: Request, res: Response, next: NextFunction)
        {            
            // Connect to mongo and update an existing document with given data
            mongoose.connect('mongodb://localhost:27017/Sift').then(() => 
            {
                Update(req);
            });

            // UserModel is a regular Mongoose Model with correct types
            async function Update(req: Request): Promise<void>
            {
                // Find the document with the appropriate symbol
                let documents: Array<InstanceType<SecurityTypeGoose>> = await SecurityModel.find( { Symbol: req.body.Symbol } ).exec();

                // Update the weight
                documents[0].Weight = req.body.Weight;

                // Save the new document document
                await documents[0].save();
            }
        });

        //Use put requests to create new resources
        this.router.put("/SECURITIES", function(req: Request, res: Response, next: NextFunction)
        {
            mongoose.connect('mongodb://localhost:27017/Sift').then(() => 
            {
                Create(req);
            });

            async function Create(req: Request): Promise<void>
            {
                // Make the document
                let document: InstanceType<SecurityTypeGoose> = new SecurityModel(req.body);

                // Save the document
                await document.save();
            }
        });

        // Delete resources from the server
        this.router.delete("/SECURITIES", function(req : Request, res : Response, next : NextFunction)
        {
            mongoose.connect('mongodb://localhost:27017/Sift').then(() => 
            {
                Delete(req);
            });

            async function Delete(req: Request): Promise<void>
            {
                SecurityModel.remove({ Symbol: req.query.Symbol }).exec();
            }
        });
    }
}