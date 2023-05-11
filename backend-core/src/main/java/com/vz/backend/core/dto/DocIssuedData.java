package com.vz.backend.core.dto;

import java.util.Calendar;
import java.util.Date;

import com.vz.backend.util.DateTimeUtils;
import com.vz.backend.util.StringUtils2;

import lombok.AllArgsConstructor;

@AllArgsConstructor
public class DocIssuedData {
    private String numberOrSign;
    private Date issuedDate;
    public String getNumberOrSign() {
        if (StringUtils2.isNullOrEmpty(this.numberOrSign)) {
            return "";
        }
        return numberOrSign;
    }

    public String getDateIssuedStr(int opt) {
        if (this.issuedDate == null)
            return "";
        Calendar cal = Calendar.getInstance(DateTimeUtils.timeZone());
        cal.setTime(this.issuedDate);
        int year = cal.get(Calendar.YEAR);
        int month = cal.get(Calendar.MONTH) + 1;
        int day = cal.get(Calendar.DAY_OF_MONTH);
        switch (opt) {
            case 1:
                return String.valueOf(year);
            case 2:
                return String.valueOf(month);
            case 3:
                return String.valueOf(day);
            default:
                break;
        }
        return "";
    }

    public boolean valids() {
        return !StringUtils2.isNullOrEmpty(this.numberOrSign);
    }
}
