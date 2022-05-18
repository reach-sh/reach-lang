import React, { useState, useEffect, useRef } from "react";
import loadingGif from "../images/ajax-loader.gif";
import { createClient } from "@supabase/supabase-js";
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faWallet } from '@fortawesome/free-solid-svg-icons';
const SUPABASE_URL = "https://byolfysahovehogqdena.supabase.co";
const SUPABASE_ANON_KEY = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6ImJ5b2xmeXNhaG92ZWhvZ3FkZW5hIiwicm9sZSI6ImFub24iLCJpYXQiOjE2NDg3NTcwMTYsImV4cCI6MTk2NDMzMzAxNn0.Q5h8nwP-qy1o5oDa0UCAgj1m7vTXOlhPyoZRC-0CNnk";
const supabaseClient = createClient(SUPABASE_URL, SUPABASE_ANON_KEY);


function ClaimNotifications({ addr }) {
    const errorFromDb = useRef("");
    const [loading, setLoading] = useState(true);
    const [claimsCount, setClaimsCount] = useState(0);

    const fetchData = async () => {
        const { data: claimArr, error: er } = await supabaseClient.from("claims").select("*");
        if (er) {
            console.log("er=", er);
            errorFromDb.current = er.message;
        } else {
            if (claimArr.length !== claimsCount) {
                setClaimsCount(claimArr.length);
            }
        }
        setLoading(false);
    }
    fetchData();

    return (
        <div className="w-full md:w-1/2 xl:w-1/3 p-3">
            {/*<!--Metric Card-->*/}
            <div className="bg-white border rounded shadow p-2">
                <div className="flex flex-row items-center">
                    <div className="flex-shrink pr-4">
                        <div className="rounded p-3 bg-green-600"><FontAwesomeIcon icon={faWallet} /></div>
                    </div>
                    <div className="flex-1 text-center md:center-center">
                        <h5 className="font-bold uppercase text-gray-500"> Claim notifications </h5>
                        <h3 className="text-2xl">
                            {
                                loading ?
                                    <span style={{ paddingTop: "1px" }}>
                                        <img src={loadingGif} width="20px" />
                                    </span>
                                    : errorFromDb.current === "" ?
                                        <>
                                            {(claimsCount > 0) &&
                                                <>
                                                    <span className="font-bold">Open claims : </span>
                                                    <span>{claimsCount} </span>
                                                </>
                                            }
                                            {(!claimsCount > 0) &&
                                                <>
                                                    <span className="font-bold">Open claims : </span>
                                                    <span> None </span><br />
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

export default ClaimNotifications;

