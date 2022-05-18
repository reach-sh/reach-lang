import { useReducer } from "react";
import { initialState, reducer } from "./Reducer";
import ConfirmContext from "./ConfirmContext";

export const ConfirmContextProvider = ({ children }) => {
    const [state, dispatch] = useReducer(reducer, initialState);

    return (
        <ConfirmContext.Provider value={[state, dispatch]}>
            {children}
        </ConfirmContext.Provider>
    );
};