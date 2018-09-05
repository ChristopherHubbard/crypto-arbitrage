import * as React from 'react';
import { Link } from 'react-router-dom';
import { connect, DispatchProp } from 'react-redux';
import { userActions } from '../actions';
import { User } from '../models';
import { Form, Field, reduxForm } from 'redux-form';

interface RegisterProps
{
    registering: boolean
}

interface RegisterState
{
    user: User,
    submitted: boolean
}

// Register page should have both props and state
class RegisterPage extends React.Component<RegisterProps & DispatchProp<any>, RegisterState>
{
    constructor(props: RegisterProps & DispatchProp<any>)
    {
        super(props);

        // Set initial state
        this.state = {
            user: {
                userName: '',
                password: '',
                phone: '',
                email: ''
            },
            submitted: false
        };
        
        // Bind functions to component
        this.handleChange = this.handleChange.bind(this);
        this.handleSubmit = this.handleSubmit.bind(this);
    }

    private handleChange(event: any): void
    {
        event = event as React.ChangeEvent<HTMLInputElement>;
        // Get the name of the property that changed and the value -- destruct object
        const { name, value } = event.target;

        // Update the state of the user -- only merges so don't have to worry about previous state
        this.setState((prevState) => ({
            user: {
                ...prevState.user,
                [name]: value
            }
        }));
    }

    private handleSubmit(event: React.ChangeEvent<HTMLInputElement>): void
    {
        // Why do I want to prevent the default action?
        event.preventDefault();

        this.setState({
            submitted: true
        });

        // Need to verify input is correct -- should try redux forms
        const { user } = this.state;
        const { dispatch } = this.props;
        // Dispatch the registration action
        dispatch(userActions.register(user));
    }

    // React render method
    public render(): React.ReactNode
    {
        const { registering } = this.props;
        const { user, submitted } = this.state;

        // Create the react node -- this is the page markup -- add validators for fields
        return (
            <div>
                <h2> Register </h2>
                <Form onSubmit={this.handleSubmit}>
                    <Field name="username" type="text" label="Username" validate={[]} onChange={this.handleChange}/>
                    <Field name="password" type="text" label="Password" validate={[]} onChange={this.handleChange}/>
                    <Field name="email" type="email" label="Email" validate={[]} onChange={this.handleChange}/>
                    <Field name="phone" type="number" label="Phone Number" validate={[]} onChange={this.handleChange}/>
                </Form>
            </div>
        );
    }
}

// Map from the store to the props
function mapStateToProps(state: any): RegisterProps
{
    // Extract the action!
    const { registering } = state.registration;
    return {
        registering
    };
}

// Connect the register page to the store and export -- autoinjects dispatch
export default connect<RegisterProps>(
    mapStateToProps
)(reduxForm({
    form: 'registerPage'
})(RegisterPage as any));