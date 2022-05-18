import React, { useState, useEffect, useRef } from "react";
import loadingGif from "../images/ajax-loader.gif";
import useConfirm from "../hooks/useConfirm";
import { createClient } from "@supabase/supabase-js";
const SUPABASE_URL = "https://byolfysahovehogqdena.supabase.co";
const SUPABASE_ANON_KEY = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6ImJ5b2xmeXNhaG92ZWhvZ3FkZW5hIiwicm9sZSI6ImFub24iLCJpYXQiOjE2NDg3NTcwMTYsImV4cCI6MTk2NDMzMzAxNn0.Q5h8nwP-qy1o5oDa0UCAgj1m7vTXOlhPyoZRC-0CNnk";
const supabaseClient = createClient(SUPABASE_URL, SUPABASE_ANON_KEY);

function ListingTable({ addr, dashboardRender, setDashboardRender, insurerContract: insurerContractHandle }) {
    const { confirm } = useConfirm();
    const [loading, setLoading] = useState(true);
    const [dataFromBackend, setDataFromBackend] = useState([]);
    const errorFromBackend = useRef("");

    useEffect(() => {
        const fetchClaimNotifications = async () => {
            const { data, error } = await supabaseClient.from('claimnotifications')
                .select(`id, responded, claimant:claimant(*), claim:claimId(*)`).match({ member: addr, responded: false });
            if (error) {
                errorFromBackend.current = error.message;
                setDataFromBackend([]);
                console.log("error while retrieving claim notifications: ", error);
            } else {
                setDataFromBackend(data);
            }
            setLoading(false);
        }
        fetchClaimNotifications();
    }, [dashboardRender, addr]);

    const respondToClaim = async ({ claimId, claimant, claimantFullName, defaultAmnt = 0, claimAmnt, claimCurrency = "UGX" }) => {
        const setAmount = prompt(`${claimantFullName} requested for ${claimCurrency} ${claimAmnt}. 
        In case you think this amount is too much or too little, 
        then enter the amount you suggest in the input box below. 
        If you don't even want to allow this person to be funded, just enter 0`, defaultAmnt);
        console.log("setAmount = ", setAmount);

        insurerContractHandle.a.CommunityMember.respondToClaim({
            claimant, accepted: true, setAmount
        }).then(async (ok) => {
            if (ok) {
                //update the approvalscount of this claim in the claims table
                const { data: updatedClaim, error } = await supabaseClient.rpc('incrementapprovalsby', { claim_id: claimId, increment_by: 1 });
                console.log("updatedClaim=", updatedClaim, "error =", error);

                let selectedClaim = {};
                if (!error) {
                    const { data: selectedClaimArr, errr } = await supabaseClient.from("claims").select("*").match({ id: claimId });
                    if ((!errr) && (selectedClaimArr.length > 0)) {
                        selectedClaim = selectedClaimArr[0];
                    }
                }
                if (selectedClaim.approvalscount && selectedClaim.approvalscount >= 5) {
                    const { error } = await supabaseClient.from("claims").delete().match({ id: claimId });
                    if (error) {
                        console.log("Failed to delete the claim after funding claimant", error);
                    }
                }
            } else {
                console.log("Oops! The backend failed to process your response to the claim.");
            }
        });

        //delete the link btn this member and the claim (ie, in the claimnotifications table), 
        //so that he will not see it again on the list of open claims
        const { error } = await supabaseClient.from("claimnotifications").delete().match({ member: addr, claimId: claimId });
        if (error) {
            console.log("Failed to delete the notification link btn member and claim", error);
        }
        setDashboardRender(!dashboardRender);
        console.log("Notification deleted");
    };

    const withdrawMyClaim = async ({ claimId }) => {
        const yes = await confirm(`Are you sure you want to withdraw your claim ?`);
        console.log("yes...");
        if (yes) {
            console.log("2yes...");
            const ok = await insurerContractHandle.apis.CommunityMember.withDrawClaim();
            console.log("withdraw calaim - OK.");
            if (ok) {
                //First delete all notifications that had been sent to all members about this claim
                const { error } = await supabaseClient.from("claimnotifications").delete().match({ claimId: claimId });
                if (error) {
                    console.log("Failed to delete the notifications for the widrawn claim", error);
                }

                //then delete the claim from the db
                const { error: er } = await supabaseClient.from("claims").delete().match({ id: claimId });
                if (er) {
                    console.log("Failed to delete the withdrawn claim", er);
                }
            } else {
                console.log("Oops! The backend failed to process your claim withdrawal.");
            }
            setDashboardRender(!dashboardRender);
        }
        console.log("withdraw calaim...");
    };

    return (
        <div className="w-full p-3">
            {/*<!--Table Card-->*/}
            <div className="bg-white border rounded shadow">
                <div className="border-b p-3">
                    <h5 className="font-bold uppercase text-gray-600"> Open claims </h5>
                </div>
                <div className="p-5" style={{ overflowX: "scroll", maxWidth: "100vw" }}>
                    <table className="w-full p-5 text-gray-700" >
                        <thead>
                            <tr>
                                <th className="text-left text-blue-900 px-2">Date</th>
                                <th className="text-left text-blue-900 px-2">Claimant</th>
                                <th className="text-left text-blue-900 px-2">Description</th>
                                <th className="text-left text-blue-900 px-2">Amount</th>
                                <th className="text-left text-blue-900 px-2">Approvals</th>
                                <th className="text-left text-blue-900 px-2">Deadline</th>
                                <th className="text-left text-blue-900 px-2">Contact</th>
                                <th className="text-left text-blue-900 px-2">Link</th>
                            </tr>
                        </thead>
                        <tbody>
                            {
                                loading ?
                                    <tr>
                                        <td colSpan="5">
                                            <img src={loadingGif} width="30px" alt="" />
                                        </td>
                                    </tr>
                                    : errorFromBackend.current !== "" ?
                                        <tr><td colSpan="6"> <p> {errorFromBackend.current} </p> </td></tr>
                                        : dataFromBackend.map((notifcn, k) =>
                                            <tr key={k}>
                                                <td className="px-2"> {notifcn.claim.created_at} </td>
                                                <td className="px-2"> {notifcn.claimant.fullName} </td>
                                                <td className="px-2"> {notifcn.claim.description} </td>
                                                <td className="px-2"> {notifcn.claim.amountRequested} </td>
                                                <td className="px-2"> {notifcn.claim.approvalscount} </td>
                                                <td className="px-2"> {notifcn.claim.deadline} </td>
                                                <td className="px-2"> {notifcn.claimant.phone}, {notifcn.claimant.email} </td>
                                                <td className="px-2">
                                                    {(notifcn.claim.claimant === addr) ?
                                                        <button
                                                            onClick={withdrawMyClaim.bind(this, { claimId: notifcn.claim.id })}
                                                            className={`bg-gray-400 py px-4 text-sm text-white rounded border border-green focus:outline-none focus:border-greenn-dark`}>
                                                            Withdraw
                                                        </button>
                                                        :
                                                        <button
                                                            onClick={respondToClaim.bind(this, { claimId: notifcn.claim.id, claimant: notifcn.claim.claimant, claimantFullName: notifcn.claimant.fullName, defaultAmnt: notifcn.claim.amountRequested, claimAmnt: notifcn.claim.amountRequested })}
                                                            className={`bg-green-500 py px-4 text-sm text-white rounded border border-green focus:outline-none focus:border-greenn-dark`}>
                                                            Respond
                                                        </button>
                                                    }
                                                </td>
                                            </tr>)
                            }
                        </tbody>
                    </table>

                </div>
            </div>
            {/*<!--/table Card-->*/}
        </div>
    );

}


export default ListingTable;
