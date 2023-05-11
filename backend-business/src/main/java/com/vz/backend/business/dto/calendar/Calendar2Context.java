package com.vz.backend.business.dto.calendar;

import java.io.IOException;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.lang.NonNull;
import org.wickedsource.docxstamper.replace.typeresolver.image.Image;

import com.vz.backend.business.domain.Calendar2;
import com.vz.backend.business.dto.Calendar2ExportDto;
import com.vz.backend.core.dto.Calendar2OrgInfo;
import com.vz.backend.core.dto.ConfigSignDto;
import com.vz.backend.util.DateTimeUtils;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class Calendar2Context {
    private Calendar startDate;
    private Calendar endDate;
    private List<Calendar2Part> calendar2Part = new ArrayList<>();
    private List<Calendar2Place> places;
    private String signer;
    private String tl;
    private String orgName;
    private String rootOrgName;
    private String position;
    private Image signImage;
    private String title;

    public Calendar2Context(Calendar2ExportDto dto, Calendar2OrgInfo orgInfo, Image signImage) throws IOException {
        this.startDate = DateTimeUtils.calendar(dto.getStartDate());
        this.endDate = DateTimeUtils.calendar(dto.getEndDate());
        this.places = new ArrayList<>();

        ConfigSignDto configSign = orgInfo.getConfig();
        for (String name : configSign.getPlace().split("\\|")) {
            this.places.add(new Calendar2Place(name));
        }
        this.signer = configSign.getSigner();
        this.tl = configSign.getTl().toUpperCase();
        this.position = configSign.getPosition().toUpperCase();
        this.orgName = orgInfo.getOrgName().toUpperCase();
        if(this.orgName!=null){
            if(this.orgName.toLowerCase().equals("ban cơ yếu chính phủ")){
                this.orgName="";
            }
        }
        this.rootOrgName = orgInfo.getRootOrgName().toUpperCase();
        if (signImage != null) {
            this.signImage= signImage;
        }else{
            Resource file = new UrlResource(getClass().getClassLoader().getResource("no-photo.jpg"));
            this.signImage = new Image(file.getInputStream(),1000);
        }
    }

    public String getDate() {
        return Integer.toString(startDate.get(Calendar.DAY_OF_MONTH));
    }

    public String getMonth() {
        return Integer.toString(startDate.get(Calendar.MONTH) + 1);
    }

    public String getYear() {
        return Integer.toString(startDate.get(Calendar.YEAR));
    }

    public String getNumber() {
        DateFormat format = DateTimeUtils.dateFormat("dd/MM/yyyy");
        return format.format(this.getStartDate().getTime());
    }

    public String getFrom() {
        DateFormat format = DateTimeUtils.dateFormat();
        return format.format(this.getStartDate().getTime());
    }

    public String getTo() {
        DateFormat format = DateTimeUtils.dateFormat();
        return format.format(this.getEndDate().getTime());
    }

    public void add(@NonNull List<Calendar2> calList) {

        this.calendar2Part = new ArrayList<>();
        calList.forEach(cal -> this.calendar2Part.add(new Calendar2Part(cal)));
        this.calendar2Part.sort((a, b) -> a.getDate().compareTo(b.getDate()));
        for (int i = 1; i < this.calendar2Part.size(); ++i) {
            Calendar2Part curr = this.calendar2Part.get(i);
            Calendar2Part last = this.calendar2Part.get(i - 1);
            if (curr.getDateStr2().compareTo(last.getDateStr2()) == 0) {
                curr.hideDateStr();
            }
        }
    }
}
