package com.vz.backend.business.domain;

import com.vz.backend.business.config.ReceiveTypeEnum;
import com.vz.backend.core.domain.BaseModel;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;

@Entity
@Table(name = "calendar2_ingredient", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class Calendar2Ingredient extends BaseModel {
    @Column(name = "object_id")
    private Long objectId;

    @Enumerated(EnumType.STRING)
    @Column(name = "type")
    private ReceiveTypeEnum type;

    @Column(name = "calendar_id")
    private long calendarId;

}
