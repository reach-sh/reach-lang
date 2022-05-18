import { useEffect, useState } from "react";

const useFetchContractInfo = ({ contractVersion }) => {
  const [contractInfo, setContractInfo] = useState("");

  const fetchContractInfo = async () => {
    try {
      const response = await fetch(`https://mahanitech.com/contractinfo/${contractVersion}`);
      const { data } = await response.json();
      setContractInfo(data);
    } catch (error) {
      setContractInfo({error: "Failed to fetch contract info, contract not yet deployed."});
    }
  };

  useEffect(() => {
    if (contractVersion) fetchContractInfo();
  }, [contractVersion]);

  return contractInfo;
};

export default useFetchContractInfo;
