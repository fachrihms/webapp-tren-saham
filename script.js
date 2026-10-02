const $=s=>document.querySelector(s);
const avg=a=>a.reduce((x,y)=>x+y,0)/a.length;
const clamp=(x,a,b)=>Math.min(b,Math.max(a,x));
const fmt=x=>isFinite(x)?x.toLocaleString('id-ID',{maximumFractionDigits:2}):'–';
const fd=ms=>new Date(ms).toISOString().slice(0,10);
let S={D:null,res:null,sel:0,rows:null};

/* ---------- parsing ---------- */
function num(v){
  if(v==null||v==='')return NaN;
  if(typeof v==='number')return v;
  let s=String(v).trim().replace(/\s/g,'').replace('%','');
  let mult=1;const m=s.match(/([KMBkmb])$/);
  if(m){mult={K:1e3,M:1e6,B:1e9}[m[1].toUpperCase()];s=s.slice(0,-1)}
  if(s===''||s==='-')return NaN;
  const hd=s.includes('.'),hc=s.includes(',');
  if(hd&&hc){ if(s.lastIndexOf(',')>s.lastIndexOf('.'))s=s.replace(/\./g,'').replace(',','.'); else s=s.replace(/,/g,''); }
  else if(hc){const p=s.split(',');if(p.length>2||(p[1].length===3&&mult===1))s=s.replace(/,/g,'');else s=s.replace(',','.');}
  else if(hd){const p=s.split('.');if(p.length>2||(p[1].length===3&&mult===1))s=s.replace(/\./g,'');}
  return parseFloat(s)*mult;
}
const MON={jan:0,feb:1,mar:2,apr:3,mei:4,may:4,jun:5,jul:6,agu:7,agt:7,aug:7,sep:8,okt:9,oct:9,nov:10,des:11,dec:11};
function pdate(v){
  if(v instanceof Date)return Date.UTC(v.getFullYear(),v.getMonth(),v.getDate());
  if(typeof v==='number'&&v>20000&&v<80000)return Math.round((v-25569)*864e5);
  const s=String(v).trim();let m;
  if(m=s.match(/^(\d{4})[-\/](\d{1,2})[-\/](\d{1,2})/))return Date.UTC(+m[1],m[2]-1,+m[3]);
  if(m=s.match(/^(\d{1,2})[-\/.](\d{1,2})[-\/.](\d{4})/))return Date.UTC(+m[3],m[2]-1,+m[1]);
  if(m=s.match(/^(\d{1,2})[ \-]([A-Za-z]{3})[A-Za-z]*[ \-,]+(\d{4})/)){const k=MON[m[2].toLowerCase()];if(k!=null)return Date.UTC(+m[3],k,+m[1]);}
  return NaN;
}
function parseCSV(t){
  t=t.replace(/^\uFEFF/,'');
  const first=t.split(/\r?\n/)[0];let best=',',bc=-1;
  for(const d of [',',';','\t']){let q=false,c=0;for(const ch of first){if(ch==='"')q=!q;else if(ch===d&&!q)c++}if(c>bc){bc=c;best=d}}
  const rows=[];let r=[],f='',q=false;
  for(let i=0;i<t.length;i++){const ch=t[i];
    if(q){if(ch==='"'){if(t[i+1]==='"'){f+='"';i++}else q=false}else f+=ch}
    else if(ch==='"')q=true;
    else if(ch===best){r.push(f);f=''}
    else if(ch==='\n'||ch==='\r'){if(ch==='\r'&&t[i+1]==='\n')i++;r.push(f);f='';if(r.some(x=>x!==''))rows.push(r);r=[]}
    else f+=ch}
  r.push(f);if(r.some(x=>x!==''))rows.push(r);
  return rows;
}
function buildData(rows){
  const H=rows[0].map(h=>String(h).toLowerCase().trim());
  const find=re=>H.findIndex(h=>re.test(h));
  const ix={d:find(/tanggal|date|waktu|time/),c:find(/terakhir|last|close|penutupan|^harga|price/),o:find(/pembukaan|open|buka/),h:find(/tertinggi|high/),l:find(/terendah|low/),v:find(/^vol/)};
  if(ix.c<0)throw new Error('Kolom harga penutupan (Terakhir / Close) tidak ditemukan di baris header.');
  let R=[];
  for(let i=1;i<rows.length;i++){const r=rows[i],c=num(r[ix.c]);if(!(c>0))continue;
    const g=k=>ix[k]>=0?num(r[ix[k]]):NaN;
    R.push({t:ix.d>=0?pdate(r[ix.d]):NaN,c,o:isFinite(g('o'))?g('o'):c,h:isFinite(g('h'))?g('h'):c,l:isFinite(g('l'))?g('l'):c,v:isFinite(g('v'))?g('v'):0});}
  const okD=R.filter(x=>isFinite(x.t)).length>=R.length*0.9;
  if(okD)R.sort((a,b)=>a.t-b.t);else R.reverse();
  return {R,okD,cols:Object.keys(ix).filter(k=>ix[k]>=0).length,
    c:R.map(x=>x.c),o:R.map(x=>x.o),h:R.map(x=>x.h),l:R.map(x=>x.l),v:R.map(x=>x.v),t:R.map(x=>x.t)};
}

/* ---------- models ---------- */
function ind(D){
  const c=D.c,n=c.length,ema=(a,p)=>{const k=2/(p+1),o=[a[0]];for(let i=1;i<a.length;i++)o.push(a[i]*k+o[i-1]*(1-k));return o};
  const rsi=Array(n).fill(50);let ag=0,al=0;
  for(let i=1;i<n;i++){const d=c[i]-c[i-1],g=Math.max(d,0),l=Math.max(-d,0);
    if(i<=14){ag+=g/14;al+=l/14}else{ag=(ag*13+g)/14;al=(al*13+l)/14}
    if(i>=14)rsi[i]=al===0?100:100-100/(1+ag/al)}
  const e12=ema(c,12),e26=ema(c,26),m=e12.map((v,i)=>v-e26[i]),sg=ema(m,9);
  D.rsi=rsi;D.mh=m.map((v,i)=>(v-sg[i])/c[i]);
}
function feat(D,i){
  const c=D.c,r=k=>c[i-k]/c[i-k-1]-1;
  return [r(0),r(1),r(2),(D.o[i]-c[i])/c[i],(D.h[i]-D.l[i])/c[i],Math.log((D.v[i]+1)/(D.v[i-1]+1)),c[i]/avg(c.slice(i-4,i+1))-1,(D.rsi[i]-50)/50,D.mh[i]];
}
function trainset(D,t){const X=[],y=[];for(let j=Math.max(30,t-500);j<t;j++){X.push(feat(D,j));y.push(D.c[j+1]/D.c[j]-1)}return{X,y}}
function stats(X){const p=X[0].length,mu=[],sd=[];for(let k=0;k<p;k++){const col=X.map(r=>r[k]),m=avg(col);mu.push(m);sd.push(Math.sqrt(avg(col.map(v=>(v-m)**2)))||1)}return{mu,sd}}
function solve(A,b){const n=b.length;for(let i=0;i<n;i++){let p=i;for(let r=i+1;r<n;r++)if(Math.abs(A[r][i])>Math.abs(A[p][i]))p=r;[A[i],A[p]]=[A[p],A[i]];[b[i],b[p]]=[b[p],b[i]];
  for(let r=i+1;r<n;r++){const f=A[r][i]/A[i][i];for(let k=i;k<n;k++)A[r][k]-=f*A[i][k];b[r]-=f*b[i]}}
  const x=Array(n).fill(0);for(let i=n-1;i>=0;i--){let s=b[i];for(let k=i+1;k<n;k++)s-=A[i][k]*x[k];x[i]=s/A[i][i]}return x}
const MODELS=[
 {name:'Naive (harga kemarin)',desc:'Baseline: besok = hari ini',f:(D,t)=>D.c[t]},
 {name:'Moving Average 5 hari',desc:'Rata-rata 5 penutupan terakhir',f:(D,t)=>avg(D.c.slice(t-4,t+1))},
 {name:'Holt (Exp. Smoothing)',desc:'Level + tren dengan pemulusan eksponensial',f:(D,t)=>{
   const c=D.c,s=Math.max(0,t-59),a=.6,b=.2;let L=c[s],T=c[s+1]-c[s];
   for(let i=s+1;i<=t;i++){const Lp=L;L=a*c[i]+(1-a)*(L+T);T=b*(L-Lp)+(1-b)*T}return L+T}},
 {name:'Linear Regression (tren 20 hari)',desc:'Garis tren OLS dari 20 penutupan terakhir',f:(D,t)=>{
   const y=D.c.slice(t-19,t+1),n=20,xm=(n-1)/2,ym=avg(y);let sxy=0,sxx=0;
   for(let i=0;i<n;i++){sxy+=(i-xm)*(y[i]-ym);sxx+=(i-xm)**2}
   const sl=sxy/sxx;return ym+sl*(n-xm)}},
 {name:'Multiple Linear Regression (Ridge)',desc:'Fitur: return, open/high/low, volume, posisi vs MA5, RSI, MACD',f:(D,t)=>{
   const{X,y}=trainset(D,t),p=X[0].length,{mu,sd}=stats(X),Z=X.map(r=>r.map((v,k)=>(v-mu[k])/sd[k])),ym=avg(y);
   const A=Array.from({length:p},()=>Array(p).fill(0)),b=Array(p).fill(0);
   Z.forEach((z,i)=>{for(let a=0;a<p;a++){b[a]+=z[a]*(y[i]-ym);for(let c=0;c<p;c++)A[a][c]+=z[a]*z[c]}});
   for(let a=0;a<p;a++)A[a][a]+=20;
   const w=solve(A,b),q=feat(D,t).map((v,k)=>(v-mu[k])/sd[k]);
   return D.c[t]*(1+clamp(ym+q.reduce((s,v,k)=>s+v*w[k],0),-.1,.1))}},
 {name:'k-Nearest Neighbors (k=7)',desc:'Cari 7 hari historis paling mirip, rata-rata return berikutnya',f:(D,t)=>{
   const{X,y}=trainset(D,t),{mu,sd}=stats(X),z=v=>v.map((x,k)=>(x-mu[k])/sd[k]),q=z(feat(D,t));
   const d=X.map((r,i)=>{const zr=z(r);return[zr.reduce((s,v,k)=>s+(v-q[k])**2,0),y[i]]}).sort((a,b)=>a[0]-b[0]).slice(0,7);
   return D.c[t]*(1+clamp(avg(d.map(x=>x[1])),-.1,.1))}},
 {name:'Random Forest (25 pohon)',desc:'Ensembel pohon keputusan (bootstrap), di-refit tiap 10 hari',f:(D,t)=>treeModel(D,t,'rf')},
 {name:'Gradient Boosting',desc:'Pohon dangkal berurutan yang memperbaiki error sebelumnya, di-refit tiap 10 hari',f:(D,t)=>treeModel(D,t,'gb')}
];
function tree(X,y,idx,d,F){
  const m=avg(idx.map(i=>y[i]));if(d===0||idx.length<10)return{v:m};
  let best=null;
  for(const k of F){const vals=idx.map(i=>X[i][k]).sort((a,b)=>a-b);
    for(let q=1;q<8;q++){const th=vals[Math.floor(q*vals.length/8)];let sl=0,nl=0,sr=0,nr=0;
      for(const i of idx){if(X[i][k]<=th){sl+=y[i];nl++}else{sr+=y[i];nr++}}
      if(nl<5||nr<5)continue;const sc=sl*sl/nl+sr*sr/nr;if(!best||sc>best.sc)best={sc,k,th}}}
  if(!best)return{v:m};
  return{k:best.k,th:best.th,l:tree(X,y,idx.filter(i=>X[i][best.k]<=best.th),d-1,F),r:tree(X,y,idx.filter(i=>X[i][best.k]>best.th),d-1,F)};
}
const tp=(n,x)=>n.k==null?n.v:tp(x[n.k]<=n.th?n.l:n.r,x);
function treeModel(D,t,kind){
  const t0=t-(t%10),key=kind+t0;D._c=D._c||{};
  if(!D._c[key]){
    const{X,y}=trainset(D,t0),idx=X.map((_,i)=>i),p=X[0].length;let s=7+t0;
    const rnd=()=>(s=(s*1664525+1013904223)%4294967296)/4294967296;
    if(kind==='rf'){const T=[];for(let b=0;b<25;b++){const bi=idx.map(()=>Math.floor(rnd()*idx.length)),F=[...Array(p).keys()].sort(()=>rnd()-.5).slice(0,4);T.push(tree(X,y,bi,4,F))}D._c[key]={T}}
    else{const base=avg(y),T=[],pr=y.map(()=>base),all=[...Array(p).keys()];
      for(let b=0;b<40;b++){const tr=tree(X,y.map((v,i)=>v-pr[i]),idx,2,all);T.push(tr);X.forEach((x,i)=>pr[i]+=.1*tp(tr,x))}D._c[key]={T,base}}
  }
  const m=D._c[key],x=feat(D,t);
  const r=kind==='rf'?avg(m.T.map(tr=>tp(tr,x))):m.base+m.T.reduce((a,tr)=>a+.1*tp(tr,x),0);
  return D.c[t]*(1+clamp(r,-.1,.1));
}
function fcTable(r,D){
  const H=+$('#hz').value,Dx={c:[...D.c],o:[...D.o],h:[...D.h],l:[...D.l],v:[...D.v]},out=[];
  for(let s=0;s<H;s++){const t=Dx.c.length-1;ind(Dx);const p=r.m.f(Dx,t),lc=Dx.c[t];Dx.c.push(p);Dx.o.push(lc);Dx.h.push(Math.max(lc,p));Dx.l.push(Math.min(lc,p));Dx.v.push(Dx.v[t]);out.push(p)}
  let d=D.okD?D.t[D.t.length-1]:NaN;const last=D.c[D.c.length-1];
  const rows=out.map((p,i)=>{let lab='Hari +'+(i+1);
    if(isFinite(d)){do{d+=864e5}while([0,6].includes(new Date(d).getUTCDay()));lab+=' <small>'+fd(d)+'</small>'}
    const w=1.96*r.rmse*Math.sqrt(i+1),ch=(p/last-1)*100;
    return `<tr><td>${lab}</td><td>${fmt(p)}</td><td class="${ch>=0?'up':'dn'}">${ch>=0?'▲':'▼'} ${fmt(ch)}%</td><td>${fmt(p-w)} – ${fmt(p+w)}</td></tr>`}).join('');
  return `<div class="card"><h2>Prediksi ${H} hari ke depan – ${r.m.name}</h2><div class="sc"><table style="min-width:420px"><thead><tr><th>Hari</th><th>Prediksi</th><th>vs hari ini</th><th>Rentang ±95%</th></tr></thead><tbody>${rows}</tbody></table></div><small>Prediksi multi-hari bersifat rekursif (hasil hari ke-n dipakai sebagai input hari berikutnya), jadi error menumpuk dan makin tidak andal untuk hari yang jauh. Skor di tabel perbandingan hanya mengukur prediksi 1 hari.</small></div>`;
}

function evaluate(D,split){
  const n=D.c.length,T0=Math.max(50,Math.floor(n*(1-split)));
  const res=MODELS.map((m,mi)=>{
    const P=[],A=[],T=[];
    for(let t=T0;t<n-1;t++){P.push(m.f(D,t));A.push(D.c[t+1]);T.push(t)}
    const mape=avg(P.map((p,i)=>Math.abs(p-A[i])/A[i]))*100,
          rmse=Math.sqrt(avg(P.map((p,i)=>(p-A[i])**2))),
          mae=avg(P.map((p,i)=>Math.abs(p-A[i])));
    let dir=NaN;if(mi>0)dir=avg(P.map((p,i)=>Math.sign(p-D.c[T[i]])===Math.sign(A[i]-D.c[T[i]])?1:0))*100;
    const next=m.f(D,n-1);
    return{m,P,A,T,mape,rmse,mae,dir,next};
  });
  const nm=res[0].mape;
  res.forEach((r,i)=>{r.err=100*clamp(1-r.mape/(2*nm),0,1);r.score=.6*r.err+.4*(i===0?50:r.dir)});
  return{res,T0,n};
}

/* ---------- render ---------- */
function chart(r,D,T0){
  const W=800,H=290,pl=58,pr=12,pt=14,pb=26,ctx=Math.min(30,T0);
  const s=T0-ctx,idx=[];for(let i=s;i<D.c.length;i++)idx.push(i);
  const last=D.c.length-1,fc=r.next,rm=r.rmse;
  const vals=idx.map(i=>D.c[i]).concat(r.P,[fc+1.96*rm,fc-1.96*rm]);
  let mn=Math.min(...vals),mx=Math.max(...vals);const pd=(mx-mn)*.06||1;mn-=pd;mx+=pd;
  const N=idx.length+1,X=i=>pl+(i-s)/(N-1)*(W-pl-pr),Y=v=>pt+(1-(v-mn)/(mx-mn))*(H-pt-pb);
  const act=idx.map(i=>X(i)+','+Y(D.c[i])).join(' ');
  const pred=r.T.map((t,i)=>X(t+1)+','+Y(r.P[i])).join(' ');
  let g='';for(let k=0;k<=4;k++){const v=mn+(mx-mn)*k/4,y=Y(v);g+=`<line x1="${pl}" x2="${W-pr}" y1="${y}" y2="${y}" stroke="var(--bd)"/><text x="${pl-6}" y="${y+4}" font-size="11" text-anchor="end" fill="var(--mut)">${fmt(v)}</text>`}
  const xl=D.okD?[fd(D.t[s]),fd(D.t[last])]:['',''];
  return `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Grafik aktual vs prediksi">${g}
  <line x1="${X(T0)}" x2="${X(T0)}" y1="${pt}" y2="${H-pb}" stroke="var(--mut)" stroke-dasharray="4 4"/>
  <text x="${X(T0)+4}" y="${pt+10}" font-size="11" fill="var(--mut)">mulai data uji</text>
  <polyline points="${act}" fill="none" stroke="var(--tx)" stroke-width="1.8"/>
  <polyline points="${pred}" fill="none" stroke="var(--ac)" stroke-width="1.6" stroke-dasharray="5 3"/>
  <line x1="${X(last+1)}" x2="${X(last+1)}" y1="${Y(fc+1.96*rm)}" y2="${Y(fc-1.96*rm)}" stroke="${fc>=D.c[last]?'var(--up)':'var(--dn)'}" stroke-width="3"/>
  <circle cx="${X(last+1)}" cy="${Y(fc)}" r="5" fill="${fc>=D.c[last]?'var(--up)':'var(--dn)'}"/>
  <text x="${pl}" y="${H-6}" font-size="11" fill="var(--mut)">${xl[0]}</text><text x="${W-pr}" y="${H-6}" font-size="11" text-anchor="end" fill="var(--mut)">${xl[1]} → besok</text></svg>
  <small>Garis hitam: harga aktual · garis putus biru: prediksi model di periode uji · titik &amp; batang: prediksi besok (±95%).</small>`;
}
function render(){
  const{D,ev}=S,{res,T0,n}=ev,last=D.c[n-1];
  const order=res.map((r,i)=>i).sort((a,b)=>res[b].score-res[a].score),best=order[0],bi=res[best];
  const sel=res[S.sel],ch=(sel.next/last-1)*100;
  const maxS=Math.max(...res.map(r=>r.score));
  const range=D.okD?`${fd(D.t[0])} s/d ${fd(D.t[n-1])}`:'(tanpa tanggal valid, urutan diasumsikan terbaru di atas)';
  let h=`<div class="card"><h2>Ringkasan</h2><div class="grid">
   <div><div class="k">Data</div><div class="v">${n} hari</div><small>${range}</small></div>
   <div><div class="k">Model paling meyakinkan</div><div class="v" style="font-size:17px">${bi.m.name}</div><small>Skor ${fmt(bi.score)}/100 · MAPE ${fmt(bi.mape)}%</small></div>
   <div><div class="k">Prediksi besok (${sel.m.name.split(' (')[0]})</div><div class="v ${ch>=0?'up':'dn'}">${fmt(sel.next)}</div><small>${ch>=0?'▲':'▼'} ${fmt(ch)}% dari ${fmt(last)} · rentang ${fmt(sel.next-1.96*sel.rmse)} – ${fmt(sel.next+1.96*sel.rmse)}</small></div>
  </div>`;
  if(best===0)h+=`<p class="msg">Tidak ada model yang konsisten mengalahkan baseline “harga kemarin” pada data uji — ini hal yang wajar untuk data saham.</p>`;
  h+=`</div><div class="card"><h2>Perbandingan model <small>(walk-forward, ${n-1-T0} hari uji)</small></h2><div class="sc"><table><thead><tr><th>Model</th><th>MAPE</th><th>RMSE</th><th>Akurasi arah</th><th>Prediksi besok</th><th>Skor keyakinan</th></tr></thead><tbody>`;
  order.forEach((i,rk)=>{const r=res[i];
   h+=`<tr class="m ${i===S.sel?'sel':''}" data-i="${i}"><td title="${r.m.desc}">${rk+1}. ${r.m.name}${rk===0?'<span class="tag">terbaik</span>':''}</td><td>${fmt(r.mape)}%</td><td>${fmt(r.rmse)}</td><td>${i===0?'<small>n/a</small>':fmt(r.dir)+'%'}</td><td>${fmt(r.next)}</td><td><span class="bar" style="width:${r.score/maxS*70}px"></span>${fmt(r.score)}</td></tr>`});
  h+=`</tbody></table></div><small>Klik baris untuk menampilkan grafiknya. <b>MAPE</b>/<b>RMSE</b>: rata-rata error (makin kecil makin baik). <b>Akurasi arah</b>: seberapa sering tebakan naik/turun benar. <b>Skor keyakinan</b> = 60% skor error (baseline naive = 50) + 40% akurasi arah. Kolom Perubahan% dihitung ulang dari harga penutupan sehingga tidak perlu konsisten dengan file.</small></div>
  <div class="card"><h2>${sel.m.name}</h2><p class="msg" style="margin-top:0">${sel.m.desc}</p>${chart(sel,D,T0)}</div>${fcTable(sel,D)}`;
  $('#out').innerHTML=h;
  document.querySelectorAll('tr.m').forEach(tr=>tr.onclick=()=>{S.sel=+tr.dataset.i;render()});
}
const MN=['Januari','Februari','Maret','April','Mei','Juni','Juli','Agustus','September','Oktober','November','Desember'];
const cmp=v=>v>=1000?Math.round(v):+v.toFixed(1);
function mchart(P,m,y){
  const dim=new Date(Date.UTC(y,m+1,0)).getUTCDate();
  if(!P.length)return `<div class="card"><h2>${tkp()}${MN[m]} ${y}</h2><small>Tidak ada data</small></div>`;
  const W=300,H=160,pl=46,pr=10,pt=10,pb=22,cs=P.map(p=>p.c);
  let mn=Math.min(...cs),mx=Math.max(...cs);const pd=(mx-mn)*.1||mx*.01||1;mn-=pd;mx+=pd;
  const X=d=>pl+(d-1)/(dim-1)*(W-pl-pr),Y=v=>pt+(1-(v-mn)/(mx-mn))*(H-pt-pb);
  const up=P[P.length-1].c>=P[0].c,col=up?'var(--up)':'var(--dn)',ch=(P[P.length-1].c/P[0].c-1)*100;
  let g='';
  for(let k=0;k<=2;k++){const v=mn+(mx-mn)*k/2,yy=Y(v);g+=`<line x1="${pl}" x2="${W-pr}" y1="${yy}" y2="${yy}" stroke="var(--bd)"/><text x="${pl-4}" y="${yy+4}" font-size="10" text-anchor="end" fill="var(--mut)">${fmt(cmp(v))}</text>`}
  [1,8,15,22,dim].forEach(d=>g+=`<text x="${X(d)}" y="${H-6}" font-size="10" text-anchor="middle" fill="var(--mut)">${d}</text>`);
  const pts=P.map(p=>X(p.d)+','+Y(p.c)).join(' ');
  const dots=P.map(p=>`<circle cx="${X(p.d)}" cy="${Y(p.c)}" r="2.6" fill="${col}"></circle>`).join('');
  return `<div class="card mc" data-m="${m}" title="Klik untuk detail"><h2>${tkp()}${MN[m]} ${y} <small class="${up?'up':'dn'}">${up?'▲':'▼'} ${fmt(ch)}%</small></h2><svg class="mch" data-m="${m}" data-pts="${P.map(p=>X(p.d).toFixed(1)+','+Y(p.c).toFixed(1)).join(';')}" viewBox="0 0 ${W} ${H}" role="img" aria-label="Harga ${MN[m]} ${y}">${g}<polyline points="${pts}" fill="none" stroke="${col}" stroke-width="1.8"/>${dots}<line class="xl" y1="${pt}" y2="${H-pb}" stroke="var(--mut)" stroke-dasharray="3 3" visibility="hidden"/><circle class="xc" r="5" fill="${col}" stroke="var(--card)" stroke-width="2" visibility="hidden"/><rect x="0" y="0" width="${W}" height="${H}" fill="transparent"/></svg></div>`;
}
function trend(){
  const D=S.D,y=+$('#yr').value,M=Array.from({length:12},()=>[]);
  D.t.forEach((t,i)=>{const d=new Date(t);if(d.getUTCFullYear()===y)M[d.getUTCMonth()].push({d:d.getUTCDate(),c:D.c[i],h:D.h[i],l:D.l[i],v:D.v[i],pc:i?(D.c[i]/D.c[i-1]-1)*100:NaN})});
  const all=M.flat();let h=`<h2 style="font-size:19px;margin:4px 0 10px">${tkp()}Tren harga ${y}</h2>`;
  if(all.length){const a=all[0].c,b=all[all.length-1].c,ch=(b/a-1)*100,cs=all.map(x=>x.c);
    h+=`<div class="card"><div class="grid"><div><div class="k">Pergerakan ${y}</div><div class="v ${ch>=0?'up':'dn'}">${ch>=0?'▲':'▼'} ${fmt(ch)}%</div><small>${fmt(a)} → ${fmt(b)}</small></div><div><div class="k">Tertinggi (penutupan)</div><div class="v">${fmt(Math.max(...cs))}</div></div><div><div class="k">Terendah (penutupan)</div><div class="v">${fmt(Math.min(...cs))}</div></div><div><div class="k">Hari bursa</div><div class="v">${all.length}</div></div></div></div>`}
  h+='<p class="msg" style="margin:0 0 8px">Klik salah satu bulan untuk melihat harga tertinggi, terendah, dan pergerakannya.</p><div class="mg">'+M.map((P,m)=>mchart(P,m,y)).join('')+'</div>';
  $('#trend').innerHTML=h;S.M=M;S.y=y;hideTip();
  $('#trend').querySelectorAll('.mc').forEach(e=>e.onclick=()=>openMonth(+e.dataset.m));
}
function openMonth(m){
  const P=S.M[m],y=S.y;if(!P||!P.length)return;
  const hi=P.reduce((a,b)=>b.h>a.h?b:a),lo=P.reduce((a,b)=>b.l<a.l?b:a),a=P[0].c,b=P[P.length-1].c,ch=(b/a-1)*100,vol=P.reduce((s,x)=>s+x.v,0);
  const up=b>=a,mo=MN[m].slice(0,3);
  const rows=P.map((x,i)=>{const pc=x.pc;return `<tr><td>${x.d} ${mo}</td><td>${fmt(x.c)}</td><td class="${pc>=0?'up':'dn'}">${isFinite(pc)?(pc>=0?'+':'')+fmt(pc)+'%':'–'}</td></tr>`}).join('');
  $('#modal').innerHTML=`<div class="box"><div class="row" style="justify-content:space-between;margin:0 0 8px"><h2 style="margin:0">${tkp()}${MN[m]} ${y}</h2><button class="g" id="mx">Tutup ✕</button></div>
  <div class="grid" style="margin-bottom:12px">
   <div><div class="k">Tertinggi</div><div class="v up">${fmt(hi.h)}</div><small>${hi.d} ${mo}</small></div>
   <div><div class="k">Terendah</div><div class="v dn">${fmt(lo.l)}</div><small>${lo.d} ${mo}</small></div>
   <div><div class="k">Pergerakan</div><div class="v ${up?'up':'dn'}">${up?'▲':'▼'} ${fmt(ch)}%</div><small>${fmt(a)} → ${fmt(b)} (${b-a>=0?'+':''}${fmt(b-a)})</small></div>
   <div><div class="k">Total volume</div><div class="v">${vol?fmt(vol):'–'}</div><small>${P.length} hari bursa</small></div></div>
  ${mchart(P,m,y).replace('card mc','card')}
  <div class="sc" style="max-height:220px;overflow:auto;margin-top:10px"><table style="min-width:320px"><thead><tr><th>Tanggal</th><th>Penutupan</th><th>Harian</th></tr></thead><tbody>${rows}</tbody></table></div></div>`;
  $('#modal').hidden=false;
}
const closeM=()=>{$('#modal').hidden=true;hideTip()};
$('#modal').onclick=e=>{if(e.target.id==='modal'||e.target.id==='mx')closeM()};
document.addEventListener('keydown',e=>{if(e.key==='Escape')closeM()});
const tip=document.createElement('div');tip.id='tip';document.body.appendChild(tip);
function hideTip(){tip.style.display='none';document.querySelectorAll('.xl,.xc').forEach(e=>e.setAttribute('visibility','hidden'))}
function hv(e){
  const svg=e.target.closest&&e.target.closest('svg.mch');
  if(!svg){hideTip();return}
  const m=+svg.dataset.m,P=S.M[m],pts=svg.dataset.pts.split(';').map(q=>q.split(',').map(Number));
  const r=svg.getBoundingClientRect(),cx=(e.clientX-r.left)/r.width*300;
  let k=0;for(let i=1;i<pts.length;i++)if(Math.abs(pts[i][0]-cx)<Math.abs(pts[k][0]-cx))k=i;
  const p=P[k],xl=svg.querySelector('.xl'),xc=svg.querySelector('.xc');
  xl.setAttribute('x1',pts[k][0]);xl.setAttribute('x2',pts[k][0]);xc.setAttribute('cx',pts[k][0]);xc.setAttribute('cy',pts[k][1]);
  xl.setAttribute('visibility','visible');xc.setAttribute('visibility','visible');
  tip.innerHTML=`<b>${tkp()}${p.d} ${MN[m]} ${S.y}</b><br>Penutupan: ${fmt(p.c)}${isFinite(p.pc)?` <span class="${p.pc>=0?'up':'dn'}">(${p.pc>=0?'+':''}${fmt(p.pc)}%)</span>`:''}<br><small>Tertinggi ${fmt(p.h)} · Terendah ${fmt(p.l)}</small>`;
  tip.style.display='block';
  const tw=tip.offsetWidth,th=tip.offsetHeight;let x=e.clientX+14,y=e.clientY-th-12;
  if(x+tw>innerWidth-8)x=e.clientX-tw-14;if(y<8)y=e.clientY+16;
  tip.style.left=x+'px';tip.style.top=y+'px';
}
document.addEventListener('pointermove',hv);document.addEventListener('pointerdown',hv);
const esc=s=>String(s??'').replace(/[&<>"]/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;'}[c]));
const tkv=()=>($('#tk').value||'').trim().toUpperCase(),tkp=()=>tkv()?esc(tkv())+' · ':'';
const STOP=new Set(['DATA','HISTORICAL','HISTORIS','HISTORY','SAHAM','STOCK','PRICE','HARGA','DOWNLOAD','FILE','COPY','EXPORT','CSV','XLSX','XLS','IDX','DATASET','LAPORAN','INVESTING','YAHOO','FINANCE','FOR','OF','THE']);
function guessTk(name){
  const base=name.replace(/\.[^.]+$/,'');
  for(const w of base.split(/[\s_()\-,]+/)){const u=w.trim().toUpperCase();
    if(/^\^?[A-Z0-9]{2,6}([.=][A-Z]{1,2})?$/.test(u)&&!/^\d+$/.test(u)&&!STOP.has(u))return u}
  return '';
}
function info(){
  const D=S.D,tk=tkv()||'Saham',n=D.c.length,last=D.c[n-1],lt=D.t[n-1];
  const atT=tt=>{if(D.t[0]>tt)return NaN;let i=0;while(i+1<n&&D.t[i+1]<=tt)i++;return(last/D.c[i]-1)*100},at=d=>atT(lt-d*864e5);
  const ly=new Date(lt).getUTCFullYear(),R=[['1 Minggu',at(7)],['1 Bulan',at(30)],['3 Bulan',at(90)],['6 Bulan',at(180)],['YTD',atT(Date.UTC(ly,0,1)-1)],['1 Tahun',at(365)]];
  const tile=(k,v)=>`<div><div class="k">${k}</div><div class="v ${v>=0?'up':'dn'}" style="font-size:18px">${isFinite(v)?(v>=0?'▲ ':'▼ ')+fmt(v)+'%':'<span style="color:var(--mut)">–</span>'}</div></div>`;
  const yr=D.t.map((t,i)=>t>=lt-365*864e5?i:-1).filter(i=>i>=0),hi=Math.max(...yr.map(i=>D.h[i])),lo=Math.min(...yr.map(i=>D.l[i])),pos=(last-lo)/(hi-lo)*100;
  const ma=k=>n>=k?avg(D.c.slice(-k)):NaN,m20=ma(20),m50=ma(50),rsi=D.rsi[n-1];
  const rets=D.c.slice(-31).map((c,i,a)=>i?c/a[i-1]-1:0).slice(1),vola=Math.sqrt(avg(rets.map(r=>r*r)))*100;
  const G=[],sg=(ok,txt,v)=>{if(ok)G.push([txt,v])};
  sg(isFinite(m20),`Harga ${last>m20?'di atas':'di bawah'} MA20 (${fmt(m20)})`,last>m20?1:-1);
  sg(isFinite(m50),`Harga ${last>m50?'di atas':'di bawah'} MA50 (${fmt(m50)})`,last>m50?1:-1);
  sg(isFinite(m50),`MA20 ${m20>m50?'di atas':'di bawah'} MA50 (${m20>m50?'tren naik':'tren turun'})`,m20>m50?1:-1);
  const r1=R[1][1],w1=R[0][1];sg(isFinite(r1),`Return 1 bulan ${r1>=0?'positif':'negatif'} (${fmt(r1)}%)`,r1>=0?1:-1);
  sg(n>20,`RSI14 ${fmt(rsi)} — ${rsi>70?'jenuh beli (rawan koreksi)':rsi<30?'jenuh jual (potensi pantulan)':rsi>=50?'momentum menguat':'momentum melemah'}`,rsi>70||rsi<30?0:rsi>=50?1:-1);
  const v5=avg(D.v.slice(-5)),v20=avg(D.v.slice(-20));
  sg(v20>0&&isFinite(w1),`Volume 5 hari ${fmt(v5/v20)}× rata-rata 20 hari`,v5/v20>1.2?(w1>=0?1:-1):0);
  const sc=G.reduce((a,x)=>a+x[1],0),lab=!G.length?['Data belum cukup','']:sc>=G.length*.4?['Bullish','up']:sc<=-G.length*.4?['Bearish','dn']:['Netral / sideways',''];
  const ic=v=>v>0?'<span class="up">✓</span>':v<0?'<span class="dn">✗</span>':'•';
  $('#info').innerHTML=`<div class="card"><h2>${esc(tk)} — performa cepat <small>per ${fd(lt)}</small></h2>
   <div class="grid"><div><div class="k">Harga terakhir</div><div class="v" style="font-size:18px">${fmt(last)}</div></div>${R.map(r=>tile(r[0],r[1])).join('')}</div>
   <p class="msg">52 minggu: terendah ${fmt(lo)} · tertinggi ${fmt(hi)} (harga sekarang di ${fmt(pos)}% dari rentang) · volatilitas harian ±${fmt(vola)}%</p></div>
  <div class="card"><h2>Sentimen teknikal <small class="${lab[1]}">${lab[0]}</small></h2>
   ${G.map(x=>`<div>${ic(x[1])} ${x[0]}</div>`).join('')}
   <p class="msg">Dihitung dari harga &amp; volume pada data yang kamu upload, bukan dari berita. ✓ sinyal positif · ✗ negatif · • netral.</p></div>
  <div class="card"><h2>Profil perusahaan <small>(AI)</small></h2><div id="aib" class="msg" style="margin:0">…</div></div>`;
  aiProfile();
}
S.ai={};
function showAI(j){
  const b=$('#aib');if(!b)return;b.className='';
  b.innerHTML=`<div class="grid"><div><div class="k">Nama</div><div class="v" style="font-size:17px">${esc(j.nama)}</div><small>${esc(j.jenis)} · ${esc(j.bursa)}</small></div><div><div class="k">Sektor</div><div class="v" style="font-size:17px">${esc(j.sektor)}</div></div><div><div class="k">Keyakinan AI</div><div class="v" style="font-size:17px">${esc(j.keyakinan)}</div></div></div>
  <p>${esc(j.bisnis)}</p><div><b>Komoditas / faktor yang memengaruhi:</b> ${(j.eksposur||[]).map(esc).join(', ')}</div>
  <div style="margin-top:4px"><b>Risiko &amp; pendorong struktural:</b> ${(j.risiko||[]).map(esc).join('; ')}</div>${j.catatan?`<p class="msg">${esc(j.catatan)}</p>`:''}
  <p class="msg">⚠️ Berdasarkan pengetahuan model AI, bukan berita terkini, dan bisa keliru atau usang — verifikasi lewat IDX / laporan resmi. Untuk berita &amp; sentimen terbaru, minta Claude mencarinya langsung di chat.</p>`;
}
async function aiProfile(){
  const b=$('#aib'),tk=tkv(),D=S.D;if(!b)return;
  if(S.demo){b.textContent='Data simulasi — tidak ada profil perusahaan.';return}
  if(!tk){b.textContent='Isi kolom ticker / nama saham di atas untuk mengenali perusahaannya.';return}
  if(S.ai[tk]){showAI(S.ai[tk]);return}
  b.textContent='Mengenali saham…';
  try{
    if(!window.puter){b.textContent='Puter.js gagal dimuat (cek koneksi atau ad-blocker).';return}
    const n=D.c.length,prompt=`Kamu asisten riset saham. Pengguna mengunggah data harga historis dengan ticker/nama: "${tk}".
Ringkasan data: penutupan terakhir ${D.c[n-1]}, rentang ${fd(D.t[0])} s/d ${fd(D.t[n-1])}, penutupan terendah ${Math.min(...D.c)}, tertinggi ${Math.max(...D.c)}.
Anggap ini saham Bursa Efek Indonesia (IDX, harga rupiah) kecuali ticker/skala harga jelas menunjukkan hal lain (komoditas, indeks, saham luar negeri).
Jawab HANYA JSON valid dengan kunci: nama (string), jenis ("saham"|"indeks"|"komoditas"|"lainnya"), bursa (string), sektor (string), bisnis (1-2 kalimat bahasa Indonesia tentang apa yang dilakukan perusahaan/instrumen), eksposur (array 2-5 string pendek: komoditas, mata uang, atau faktor makro yang paling memengaruhi harga), risiko (array 2-3 string: risiko/pendorong struktural umum), keyakinan ("tinggi"|"sedang"|"rendah"), catatan (string singkat).
Jika tidak yakin ticker itu apa, set keyakinan "rendah" dan jangan mengarang nama perusahaan. Kamu TIDAK punya akses berita terkini; jangan menyebut berita atau kejadian terbaru.`;
    const r=await puter.ai.chat(prompt,{model:'openai/gpt-5.5'});
    const txt=typeof r==='string'?r:(Array.isArray(r?.message?.content)?r.message.content.map(c=>c.text||'').join(''):(r?.message?.content||r?.text||String(r)));
    const mm=txt.match(/\{[\s\S]*\}/);if(!mm)throw new Error('Respons AI tidak berisi JSON');
    const j=JSON.parse(mm[0]);
    S.ai[tk]=j;if(tkv()===tk)showAI(j);
  }catch(e){const x=$('#aib');if(x)x.textContent=e&&e.code==='not_granted'?'Izin AI belum diberikan, jadi profil perusahaan tidak ditampilkan.':'Gagal memuat profil: '+((e&&(e.message||(e.error&&e.error.message)))||'error')}
}
$('#tk').onchange=()=>{if(S.D){trend();info()}};
function calc(){
  const D=S.D,o=$('#out');if(!D||!$('#pred').open||S.ev)return;
  if(D.c.length<80){o.innerHTML='<p class="msg err">Fitur prediksi butuh minimal 80 hari data.</p>';return}
  o.innerHTML='<p class="msg">Menghitung…</p>';
  setTimeout(()=>{S.ev=evaluate(D,+$('#sp').value/100);S.sel=S.ev.res.map((r,i)=>i).sort((a,b)=>S.ev.res[b].score-S.ev.res[a].score)[0];render()},20);
}
function run(rows){
  const msg=$('#msg');msg.className='msg';
  try{
    const D=buildData(rows);
    if(D.c.length<2)throw new Error('Data harga tidak terbaca. Pastikan ada header kolom Tanggal dan Terakhir/Close.');
    if(!D.okD)throw new Error('Kolom tanggal tidak terbaca. Grafik bulanan butuh kolom Tanggal (mis. 31/12/2024, 2024-12-31, atau 31 Des 2024).');
    ind(D);S.D=D;S.ev=null;$('#out').innerHTML='';$('#pred').open=false;
    const ys=[...new Set(D.t.map(t=>new Date(t).getUTCFullYear()))].sort((a,b)=>b-a);
    $('#yr').innerHTML=ys.map(y=>`<option>${y}</option>`).join('');$('#yrbar').hidden=false;
    msg.textContent=`Terbaca ${D.c.length} baris (${fd(D.t[0])} s/d ${fd(D.t[D.t.length-1])}), ${ys.length} tahun.`;
    trend();info();
  }catch(e){msg.className='msg err';msg.textContent=e.message;$('#trend').innerHTML='';$('#out').innerHTML='';$('#yrbar').hidden=true;$('#info').innerHTML='';S.D=null}
}
function handle(file){
  S.demo=false;const g=guessTk(file.name);if(g)$('#tk').value=g;
  const ext=file.name.split('.').pop().toLowerCase(),fr=new FileReader();
  if(ext==='xlsx'||ext==='xls'){
    fr.onload=e=>{try{const wb=XLSX.read(e.target.result,{type:'array',cellDates:true}),ws=wb.Sheets[wb.SheetNames[0]];
      S.rows=XLSX.utils.sheet_to_json(ws,{header:1,raw:true,defval:''}).filter(r=>r.some(x=>x!==''));run(S.rows)}catch(err){$('#msg').className='msg err';$('#msg').textContent='Gagal membaca Excel: '+err.message}};
    fr.readAsArrayBuffer(file);
  }else{fr.onload=e=>{S.rows=parseCSV(e.target.result);run(S.rows)};fr.readAsText(file)}
}
function demo(){
  let s=42;const rnd=()=>(s=(s*1664525+1013904223)%4294967296)/4294967296;
  const rows=[['Tanggal','Terakhir','Pembukaan','Tertinggi','Terendah','Vol.','Perubahan%']];
  let p=5000,d=Date.UTC(2024,0,2);
  for(let i=0;i<400;i++){const o=p,r=(rnd()-.5)*.05+.0003;p=Math.round(p*(1+r)/5)*5;
    const hi=Math.max(o,p)*(1+rnd()*.01),lo=Math.min(o,p)*(1-rnd()*.01);
    while([0,6].includes(new Date(d).getUTCDay()))d+=864e5;
    rows.push([fd(d),p,o,Math.round(hi),Math.round(lo),Math.round(2e7+rnd()*8e7),r*100]);d+=864e5}
  S.demo=true;$('#tk').value='SIMULASI';S.rows=rows;run(rows);$('#msg').textContent='Memakai data simulasi acak (bukan saham sungguhan) — hanya untuk mencoba tampilan.';
}
const dz=$('#drop'),fi=$('#file');
dz.onclick=()=>fi.click();fi.onchange=()=>fi.files[0]&&handle(fi.files[0]);
dz.ondragover=e=>{e.preventDefault();dz.classList.add('on')};dz.ondragleave=()=>dz.classList.remove('on');
dz.ondrop=e=>{e.preventDefault();dz.classList.remove('on');e.dataTransfer.files[0]&&handle(e.dataTransfer.files[0])};
$('#demo').onclick=demo;
$('#sp').oninput=e=>{$('#spv').textContent=e.target.value+'%'};
$('#sp').onchange=()=>{S.ev=null;calc()};
$('#yr').onchange=trend;
$('#pred').ontoggle=calc;
$('#hz').onchange=()=>S.ev&&render();
