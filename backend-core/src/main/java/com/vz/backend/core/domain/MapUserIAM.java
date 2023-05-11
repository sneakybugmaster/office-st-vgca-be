package com.vz.backend.core.domain;

import lombok.*;

import javax.persistence.*;

@Entity
@Table(name = "MAP_USER_IAM", schema = "vz")
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class MapUserIAM extends BaseModel{
    @Column(name = "sub_id")
    private String subId;

    @Column(name = "user_id")
    private Long userId;

    @OneToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "user_id", insertable = false, updatable = false)
    private User user;

    @Column(name = "check_user")
    private Boolean checkUser;
}
