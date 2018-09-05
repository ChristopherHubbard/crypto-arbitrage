import * as React from 'react';
import { connect, DispatchProp } from 'react-redux';
import { User } from '../models';

interface HomeProps
{
    user: User
}

interface HomeState
{

}

class HomePage extends React.Component<HomeProps & DispatchProp<any>, HomeState>
{
    constructor(props: HomeProps & DispatchProp<any>)
    {
        super(props);
    }

    public render(): React.ReactNode
    {
        return (
            <div>
                <h1> Home Page </h1>
            </div>
        );
    }
}

function mapStateToProps(state: any): HomeProps
{
    const { user } = state.authentication;
    return {
        user
    };
}

export default connect<HomeProps>(
    mapStateToProps
)(HomePage);