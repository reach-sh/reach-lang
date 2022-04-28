import React from "react";
import {Form, Field, ObjectField, FeedbackStrategies} from "formula-one";

type UInt<T extends number> =
    number extends T 
        ? never 
        : `${T}` extends `-${string}` | `${string}.${string}`
            ? never 
            : T;

type FieldDataType = {
    [key: string]: UInt<number> | string
}

type ObjectSet = {

}
export default function SimpleExample(values: any) {
  return (
    <div className="App">
      <Form
        initialValue={[] as FieldDataType[]}
        onSubmit={person => console.log("Submitted", person)}
      >
        {(link, onSubmit) => (
            
          <ObjectField link={link}>
            {(links) => links.map((link, index) => 
              <>(
                <Field link={link}>
                  {(value, errors, onChange) => (
                    <label>
                      <div>Name</div>
                      <input
                        type="text"
                        value={values[index]}
                        onChange={e => onChange(e.target.value)}
                      />
                    </label>
                  )}
                </Field>
                <div>
                  <button onClick={onSubmit}>Submit</button>
                </div>
              </>)
            }
          </ObjectField>
        )}
      </Form>
    </div>
  );
}