export const SHOW_CONFIRM = 'SHOW_CONFIRM';
export const HIDE_CONFIRM = 'HIDE_CONFIRM';

export const initialState = {
    show: false,
    text: ''
};

export const reducer = (state = initialState, action) => {
    switch (action.type) {
        case SHOW_CONFIRM:
            return {
                show: true,
                text: action.payload.text
            };
        case HIDE_CONFIRM:
            return initialState;
        default:
            return initialState;
    }
};