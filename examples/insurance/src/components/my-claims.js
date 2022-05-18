import React, { useState, useRef } from "react";
import loadingGif from "../images/ajax-loader.gif";
import { createClient } from "@supabase/supabase-js";
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faWallet } from '@fortawesome/free-solid-svg-icons';
const SUPABASE_URL = "https://byolfysahovehogqdena.supabase.co";
const SUPABASE_ANON_KEY = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6ImJ5b2xmeXNhaG92ZWhvZ3FkZW5hIiwicm9sZSI6ImFub24iLCJpYXQiOjE2NDg3NTcwMTYsImV4cCI6MTk2NDMzMzAxNn0.Q5h8nwP-qy1o5oDa0UCAgj1m7vTXOlhPyoZRC-0CNnk";
const supabaseClient = createClient(SUPABASE_URL, SUPABASE_ANON_KEY);


function MyClaims({ addr, setShowNewClaimForm, showNewClaimForm }) {
    const myclaim = useRef({ amountRequested: 0, approvalsCount: 0 });
    const errorFromDb = useRef("");
    const [loading, setLoading] = useState(true);
    const [hasClaim, setHasClaim] = useState(false);
    const [openNewClaimForm, setOpenNewClaimForm] = useState(false);

    const fetchMyClaimData = async () => {
        const { data: claimArr, error: er } = await supabaseClient.from("claims").select("*").match({ claimant: addr });
        if (er) {
            errorFromDb.current = er.message;
        } else {
            if (claimArr.length > 0) {
                myclaim.current = claimArr[0];
                if (!hasClaim) {
                    setHasClaim(true);
                }
            } else {
                if (hasClaim) {
                    setHasClaim(false);
                }
            }
        }
        setLoading(false);
    }
    fetchMyClaimData();

    const showNewClaimFormm = async () => {
        setShowNewClaimForm(!openNewClaimForm);
        setOpenNewClaimForm(!openNewClaimForm);
    };

    return (
        <div className="w-full md:w-1/2 xl:w-1/3 p-3">
            {/*<!--Metric Card-->*/}
            <div className="bg-white border rounded shadow p-2">
                <div className="flex flex-row items-center">
                    <div className="flex-shrink pr-4">
                        <div className="rounded p-3 bg-green-600"><FontAwesomeIcon icon={faWallet} /></div>
                    </div>
                    <div className="flex-1 text-center md:center-center">
                        <h5 className="font-bold uppercase text-gray-500"> {(!hasClaim) && <button onClick={showNewClaimFormm} className="bg-blue-300 py px-4 text-sm text-white rounded border border-green focus:outline-none focus:border-greenn-dark">create new</button>} insurance claim </h5>
                        <h3 className="text-xl">
                            {
                                loading ?
                                    <span style={{ paddingTop: "1px" }}>
                                        <img src={loadingGif} width="20px" alt="" />
                                    </span>
                                    : errorFromDb.current === "" ?
                                        <>
                                            {hasClaim &&
                                                <>
                                                    <span className="font-bold">Claim status : </span>
                                                    <span>{myclaim.current.approvalscount} aproval{(myclaim.current.approvalscount > 1) ? "s" : ""}</span><br />
                                                    <span>Amount requested : {myclaim.current.amountRequested}</span>
                                                </>
                                            }
                                            {(!hasClaim) &&
                                                <>
                                                    <span className="font-bold">Claim status : </span>
                                                    <span> No open claim </span><br />
                                                </>
                                            }
                                        </>
                                        : <small> {errorFromDb.current} </small>
                            }
                        </h3>
                    </div>
                </div>
            </div>
            {/*<!--/Metric Card-->*/}
        </div>
    );

}

export default MyClaims;

