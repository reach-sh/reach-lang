import React, { useState } from "react";
import defaultUserPicture from "../images/placeholder_avatar.png";
import logo from "../images/property-img-illust.png";
import refreshIcon from "../images/refresh.png";

function Header({ currentUser, refreshPage }) {
    const [userMenuActive, setUserMenuActive] = useState(false);
    const [navMenuActive, setNavMenuActive] = useState(false);

    function toggleUserMenuDiv() {
        setUserMenuActive(!userMenuActive);
    }

    function toggleNavMenuDiv() {
        setNavMenuActive(!navMenuActive);
    }

    function disconnectWallet() {
        alert("Disconnecting...");
        //
    }

    return (
        <>
            <nav id="header" className="fixed w-full z-10 top-0 shadow pb-4 bg-gradient-to-t from-white to-green-300">

                <div className="w-full container mx-auto flex flex-wrap items-center mt-0 pt-3 pb-3 md:pb-0">

                    <div className="w-1/2 pl-2 md:pl-0">
                        <a className="text-gray-900 text-base xl:text-xl no-underline hover:no-underline font-bold" href="#">
                            <table>
                                <tbody>
                                    <tr>
                                        <td>
                                            <img src={logo} width="40px" alt="" />
                                        </td>
                                        <td>
                                            <span style={{ fontSize: "3vw" }}> Insurance Dapp </span>
                                        </td>
                                    </tr>
                                </tbody>
                            </table>
                        </a>
                    </div>
                    <div className="w-1/2 pr-0">
                        <div className="flex relative inline-block float-right">

                            <div className="relative text-sm">
                                <button className="flex items-center focus:outline-none mr-3" onClick={refreshPage}>
                                    <img className="w-8 h-8 rounded-full mr-4" src={refreshIcon} alt="refresh" />
                                </button>
                            </div>
                            <div className="relative text-sm">
                                <button className="flex items-center focus:outline-none mr-3" onClick={toggleUserMenuDiv}>
                                    <img className="w-8 h-8 rounded-full mr-4 border-solid border-2 border-green-600" src={defaultUserPicture} alt="User" />
                                    <span className="hidden md:inline-block"> {currentUser.fullName} </span>
                                </button>
                                <div className={`bg-white rounded shadow-md mt-2 absolute mt-12 top-0 right-0 min-w-full overflow-auto z-30 
                                    ${!userMenuActive && "invisible"}
                                `}>
                                    <ul className="list-reset">
                                        <li><a href="#" onClick={toggleUserMenuDiv} className="px-4 py-2 block text-gray-900 hover:bg-gray-400 no-underline hover:no-underline">My account</a></li>
                                        <li><a href="#" onClick={toggleUserMenuDiv} className="px-4 py-2 block text-gray-900 hover:bg-gray-400 no-underline hover:no-underline">Manage account</a></li>
                                        <li>
                                            <hr className="border-t mx-2 border-gray-400" />
                                        </li>
                                        <li >
                                            <a href="#" onClick={disconnectWallet} className="px-4 py-2 block text-gray-900 hover:bg-gray-400 no-underline hover:no-underline">Disconnect</a>
                                        </li>
                                    </ul>
                                </div>
                            </div>


                            <div className="block lg:hidden pr-4">
                                <button onClick={toggleNavMenuDiv} id="nav-toggle" className="flex items-center px-3 py-2 border rounded text-gray-500 border-gray-600 hover:text-gray-900 hover:border-teal-500 appearance-none focus:outline-none">
                                    <svg className="fill-current h-3 w-3" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg">
                                        <title>Menu</title>
                                        <path d="M0 3h20v2H0V3zm0 6h20v2H0V9zm0 6h20v2H0v-2z" />
                                    </svg>
                                </button>
                            </div>
                        </div>

                    </div>


                    <div className={`w-full flex-grow lg:flex lg:items-center lg:w-auto ${!navMenuActive && "hidden"}  lg:block mt-2 lg:mt-0 bg-white z-20`}>
                        <ul className="list-reset lg:flex flex-1 items-center px-4 md:px-0">

                        </ul>

                        <div className="relative pull-right pl-4 pr-4 md:pr-0">

                        </div>

                    </div>

                </div>
            </nav>
        </>
    );

}

export default Header;













