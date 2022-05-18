import React, { useState } from "react";

function SignupForm({ algoAccount, setActivePage, backend, contractInfo }) {
    const [fullname, setFullname] = useState("");
    const [email, setEmail] = useState("");
    const [phone, setPhone] = useState("");
    const [insrPackage, setInsrPackage] = useState("");
    const [isRegisteringMember, setIsRegisteringMember] = useState(false);

    //===============================================================
    async function Signup(e) {
        e.preventDefault();
        console.log("Signup() invoked");
        setIsRegisteringMember(true);
        if (!email || email === "") {
            alert("Email is required");
        } else if (!phone || phone === "") {
            alert("Phone number is required");
        } else if (!fullname || fullname === "") {
            alert("Full name number is required");
        } else if (!insrPackage || insrPackage === "") {
            alert("Please select your insurance package");
        }

        //if we are here, then contractInfo was retrieved earlier by useEffect(()=>{ ... }) above
        const insurerContractHandle = algoAccount.current.contract(backend, contractInfo.current);
        console.log("Signup: insurerContractHandle = ", insurerContractHandle);

        const success = await insurerContractHandle.apis.CommunityMember.registerMembership({
            fullName: fullname, phone, email, chosenInsurancePackage: insrPackage
        });
        console.log(">Register success=", success);

        if (success) {
            console.log("Registered successfully.");
            setActivePage("DASHBOARD");
        } else {
            console.log("Failed to register");
        }
        setIsRegisteringMember(false);
    }
    //===============================================================

    return (
        <div className='h-screen flex bg-blue-100'>
            <div className='w-full max-w-md m-auto bg-white rounded-lg border border-primaryBorder shadow-default py-2 px-16 shadow'>
                <h1 className='text-4xl text-blue-700  text-primary mt-2 mb-2 text-center'> Insurance Dapp </h1>
                <hr />
                <small> Please keep your mnemonic secret: ( ... ) </small>
                <hr />

                <h1 className='text-xl font-medium text-primary mt-6 mb-6 text-center'>
                    Register for insurance services
                </h1>

                <form >
                    <div>
                        <label htmlFor='fullname'>Full name </label>
                        <input
                            type="text"
                            value={fullname}
                            onChange={e => setFullname(e.target.value)}
                            className={`w-full py px-2 text-primary border rounded-md outline-none text-sm transition duration-150 ease-in-out mb-4`}
                            id='fullname'
                        />
                    </div>

                    <div>
                        <label htmlFor='phone'>Phone number </label>
                        <input
                            type="text"
                            value={phone}
                            onChange={e => setPhone(e.target.value)}
                            className={`w-full py px-2 text-primary border rounded-md outline-none text-sm transition duration-150 ease-in-out mb-4`}
                            id='phone'
                        />
                    </div>

                    <div>
                        <input
                            type="email"
                            value={email}
                            onChange={e => setEmail(e.target.value)}
                            className={`w-full py px-2 text-primary border rounded-md outline-none text-sm transition duration-150 ease-in-out mb-4`}
                            placeholder='your@email.com'
                        />
                    </div>

                    <div>
                        <label htmlFor='phone'> Select the insurance package you prefer </label>
                        <select
                            type="select"
                            value={insrPackage}
                            onChange={e => setInsrPackage(e.target.value)}
                            className={`w-full py px-2 text-primary border rounded-md outline-none text-sm transition duration-150 ease-in-out mb-4`}
                        >
                            <option value={0}> Select a package </option>
                            <option value={1}> Package-1 - 1000 - 120,000 </option>
                            <option value={2}> Package-2 - 5000 - 600,000 </option>
                            <option value={3}> Package-3 - 10,000 - 1,200,000 </option>
                            <option value={4}> Package-4 - 50,000 - 6,000,000 </option>
                            <option value={5}> Package-5 - 100,000 - 12,000,000 </option>
                            <option value={6}> Package-6 - 500,000 - 60,000,000 </option>
                            <option value={7}> Package-7 - 1,000,000 - 120,000,000 </option>
                            <option value={8}> Package-8 - 5,000,000 - 600,000,000 </option>
                        </select>
                    </div>

                    <div className='flex justify-center items-center mt-6'>
                        {isRegisteringMember ?
                            <button onClick={Signup}
                                className={`bg-blue-200 py-2 px-4 text-sm text-white rounded border border-green focus:outline-none focus:border-greenn-dark`}
                            >
                                Registering ...
                            </button>
                            :
                            <button onClick={Signup}
                                className={`bg-blue-500 py-2 px-4 text-sm text-white rounded border border-green focus:outline-none focus:border-greenn-dark`}
                            >
                                Register now
                            </button>
                        }
                    </div>
                </form>
            </div>
        </div >
    );
}

export default SignupForm;
