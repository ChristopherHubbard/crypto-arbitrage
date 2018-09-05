import * as React from 'react';
import { DispatchProp } from 'react-redux';
import { connect } from 'react-redux';

interface LoginProps
{

}

interface LoginState
{

}

class LoginPage extends React.Component<LoginProps & DispatchProp<any>, LoginState>
{
    constructor(props: LoginProps & DispatchProp<any>)
    {
        super(props);
    }

    public render(): React.ReactNode
    {
        return (
            <div>
                <h1> Login Page </h1>
            </div>
        );
    }
}

function mapStateToProps(state: any): LoginProps
{
    return {

    };
}

export default connect<LoginProps>(
    mapStateToProps
)(LoginPage);