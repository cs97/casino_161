use axum::{
    extract::{Path, State},
    http::StatusCode,
    response::IntoResponse,
    routing::{get, post},
    Json, Router,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use tower_http::services::ServeDir;

// Definition des gemeinsamen App-State
type Db = Arc<RwLock<HashMap<String, i32>>>;

// Payloads für die JSON-Kommunikation
#[derive(Deserialize)]
struct UpdateScore {
    score: i32,
}

#[derive(Serialize)]
struct ScoreResponse {
    username: String,
    score: i32,
}

#[tokio::main]
async fn main() {
    // Shared State initialisieren (In-Memory DB)
    let shared_state: Db = Arc::new(RwLock::new(HashMap::new()));

	let serve_dir = ServeDir::new("./");

    // Routen definieren
    let app = Router::new()
        .route("/score/:username", get(get_score))
        .route("/score/:username", post(update_score))
		.route("/ping", get(ping))
		.fallback_service(serve_dir)
        .with_state(shared_state);

    // Server starten
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3030")
        .await
        .unwrap();
    println!("Server läuft auf http://0.0.0.0:3030");
    axum::serve(listener, app).await.unwrap();
}

// Handler: Punktestand abfragen
async fn get_score(
    Path(username): Path<String>,
    State(state): State<Db>,
) -> impl IntoResponse {
    let db = state.read().unwrap();
    
    if let Some(&score) = db.get(&username) {
        let response = ScoreResponse { username, score };
        (StatusCode::OK, Json(response)).into_response()
    } else {
        (StatusCode::NOT_FOUND, format!("User '{}' nicht gefunden", username)).into_response()
    }
}

// Handler: Punktestand aktualisieren oder erstellen
async fn update_score(
    Path(username): Path<String>,
    State(state): State<Db>,
    Json(payload): Json<UpdateScore>,
) -> impl IntoResponse {
    let mut db = state.write().unwrap();
    
    // Punktestand einfügen oder überschreiben
    db.insert(username.clone(), payload.score);
    
    let response = ScoreResponse {
        username,
        score: payload.score,
    };

    (StatusCode::OK, Json(response))
}

async fn ping() -> &'static str {
    "pong"
}
