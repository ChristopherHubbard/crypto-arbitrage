import * as React from 'react';
import { Router, Route, HashRouter } from 'react-router-dom';
import RegisterPage from './RegisterPage';
import LoginPage from './LoginPage';
import HomePage from './HomePage';
import { PrivateRoute } from './PrivateRoute';

export class App extends React.Component<{}, {}>
{
    constructor(props: any)
    {
        super(props);
    }

    public render(): React.ReactNode
    {
        return (
            <div className="jumbotron">
                <div className="container">
                    <div className="col-sm-8 col-sm-offset-2">
                        <HashRouter>
                            <div>
                                <PrivateRoute path="/" component={HomePage}/>
                                <Route path="/login" component={LoginPage}/>
                                <Route path="/register" component={RegisterPage}/>
                            </div>
                        </HashRouter>
                    </div>
                </div>
            </div>
        );
    }
}