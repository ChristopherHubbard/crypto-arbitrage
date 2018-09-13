import { NextFunction, Request, Response } from "express";

// Import base route class
import { CustomRouter } from "./CustomRouter";

// Temp data store
let users: Array<any> = [];

// Defines the routes used at the index of the application
export class UserRouter extends CustomRouter
{
    // Implement the route creating method
    protected CreateRoutes(): void
    {
        this.router.post('/register', function(req: Request, res: Response, next: NextFunction): void
        {
            // Hash and salt the password -- make sure all the info arrived
            const user = req.body.body;

            if (user.username === '' || user.password === '' || user.phone === '' || user.email === '')
            {
                // Send no authorization response
                res.status(401).send();
            }
            else
            {
                // Save user to the DB
                users.push(user);

                // Respond no content success
                res.status(204).send();
            }
        });

        this.router.get('/authenticate', function(req: Request, res: Response, next: NextFunction): void
        {
            const currentUser = req.query;
            const user = users.find((user: any) =>
            {
                return user.username === currentUser.username && user.password === currentUser.password;
            });

            // Create an authentication token to verify that the user is real
            if (user)
            {
                user['token'] = true;
            }

            res.status(200).send(user);
        });
    }
}