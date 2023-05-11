package com.vz.backend.business.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class DocOutValueSyncDto {
    private String chuc_vu;
    private String co_quan_ban_hanh;
    private String do_khan;
    private String doc_type;
    private String lang;
    private String nam;
    private String nguoi_ky;
    private DocOutValueDateSyncDto ngay_ban_hanh;
    private DocOutValueDateSyncDto ngay_den;
    private ArrayList<String> noi_nhan_ben_ngoai;
    private ArrayList<String> noi_nhan_ben_trong;
    private String so_den;
    private String so_ky_hieu;
    private String so_trang;
    private String ten_van_ban;
    private String trich_yeu;
}
