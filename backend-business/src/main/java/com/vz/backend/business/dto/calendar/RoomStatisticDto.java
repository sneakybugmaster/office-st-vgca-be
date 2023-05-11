package com.vz.backend.business.dto.calendar;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@NoArgsConstructor
@Getter
@Setter
public class RoomStatisticDto {
    private String roomName;
    private String address;
    private Long quantityMeeting;

    public RoomStatisticDto(String roomName, String address, Long quantityMeeting) {
        this.roomName = roomName;
        this.address = address;
        this.quantityMeeting = quantityMeeting;
    }
}
